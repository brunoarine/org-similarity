import argparse
import functools
import os
import re
import sys
from pathlib import Path
from typing import Callable, List

import numpy as np
import orgparse
from nltk.stem import SnowballStemmer, WordNetLemmatizer
from scipy import sparse
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.metrics.pairwise import cosine_similarity

STOPWORDS_FILENAME = "stopwords.txt"
JUNKCHARS_FILENAME = "junkchars.txt"
FILES_EXT = "*.org"
SCRIPT_PATH = Path(__file__).parent


class Processor:
    """Class containing preprocessing and tokenization rules.

    Args:
        junkchars (list): List of junk characters to be stripped from the text.
        stopwords (list): List of stopwords to be removed from the text.
        stemmer (nltk's stemmer): Stemmer provided by the nltk API.
        lemmatize (bool): Whether to lemmatize tokens.
    """

    def __init__(
        self,
        junkchars: List[str],
        stopwords: List[str],
        stemmer: Callable,
        lemmatize=False,
    ):
        self.junkchars = junkchars
        self.stopwords = stopwords
        self.stemmer = stemmer
        self.lemmatize = lemmatize
        self._lemmatizer = WordNetLemmatizer() if self.lemmatize else None

        self._stopwords_re = re.compile(
            r"\b(" + r"|".join(stopwords) + r")\b\s*"
        )
        self._word_re = re.compile(r"(?u)\b[a-z]{2,}\b")
        self._org_property_re = re.compile(r"#\+\w+:")
        self._org_tags_re = re.compile(r":[\w:]*:")
        self._url_re = re.compile(r"\S*https?:\S*")

    def preprocessor(self, text: str) -> str:
        """Remove fancy symbols and stopwords."""
        text = text.lower()
        text = text.translate({ord("’"): ord("'")})
        text = self._org_property_re.sub("", text)
        text = self._org_tags_re.sub("", text)
        text = self._stopwords_re.sub("", text)
        text = self._url_re.sub("", text)
        return text

    def _tokenize(self, text: str) -> List[str]:
        """Preprocess a text and returns a list of tokens."""
        words = self._word_re.findall(text)
        return words

    def _lemmatize(self, tokens: List[str]) -> List[str]:
        return [self._lemmatizer.lemmatize(w) for w in tokens]

    def _stemmize(self, tokens: List[str]) -> List[str]:
        """Get only the stems from a list of words."""
        return [self.stemmer(w) for w in tokens]

    def tokenizer(self, text: str) -> List[str]:
        """Run the preprocessor."""

        tokens = self._tokenize(text)
        tokens = self._lemmatize(tokens) if self.lemmatize else tokens
        tokens = self._stemmize(tokens)
        return tokens


class Tfidf:
    """Scikit-learn's TF-IDF wrapper.

    Args:
        processor (Processor): Processor object.
        b (float): Free parameter. Default is 0.75.
        k1 (float): Free parameter. Recommended value is between 1.2 and 2.0.
        normalize (bool): Divide the results by the maximum value so it sits
            in the range between 0 and 1.
    """

    def __init__(self, processor, **kwargs):
        self.processor = processor

        self._vectorizer = TfidfVectorizer(
            tokenizer=self.processor.tokenizer,
            preprocessor=self.processor.preprocessor,
            token_pattern=None,
            **kwargs,
        )

    def fit(self, documents: List[str]):
        self._vectorizer.fit(documents)
        self.documents_embeddings_ = self._vectorizer.transform(documents)

    def get_scores(self, source: str):
        self.source_embeddings_ = self._vectorizer.transform([source])
        scores = cosine_similarity(
            self.source_embeddings_, self.documents_embeddings_
        ).flatten()
        return scores


class BM25:
    """Implementation of OKapi BM25 with Scikit-learn's TfidfVectorizer.

    Author: Yuta Koreeda
    URL: https://gist.github.com/koreyou/f3a8a0470d32aa56b32f198f49a9f2b8

    Args:
        processor (Processor): Processor object.
        b (float): Free parameter. Default is 0.75.
        k1 (float): Free parameter. Recommended value is between 1.2 and 2.0.
        normalize (bool): Divide the results by the maximum value so it sits
            in the range between 0 and 1.
    """

    def __init__(self, processor, b=0.75, k1=1.6, normalize=True, **kwargs):
        self.processor = processor
        self.normalize = normalize

        self._vectorizer = TfidfVectorizer(
            tokenizer=self.processor.tokenizer,
            preprocessor=self.processor.preprocessor,
            token_pattern=None,
            norm=None,
            smooth_idf=False,
            **kwargs,
        )
        self.b = b
        self.k1 = k1

    def fit(self, documents: List[str]):
        """Fit IDF to documents X"""
        self._vectorizer.fit(documents)
        self.embeddings_ = self._vectorizer.transform(documents)
        self.avdl = self.embeddings_.sum(1).mean()

    def get_scores(self, source: str):
        """Calculate BM25 between query q and documents X"""
        b, k1, avdl = self.b, self.k1, self.avdl

        len_X = self.embeddings_.sum(1).A1
        (q,) = self._vectorizer.transform([source])
        assert sparse.isspmatrix_csr(q)

        # Convert to csc for better column slicing
        csc_embeddings_ = self.embeddings_.tocsc()[:, q.indices]
        denom = csc_embeddings_ + (k1 * (1 - b + b * len_X / avdl))[:, None]
        # idf(t) = log [ n / df(t) ] + 1 in sklearn, so it need to be coneverted
        # to idf(t) = log [ n / df(t) ] with minus 1
        idf = self._vectorizer._tfidf.idf_[None, q.indices] - 1
        numer = csc_embeddings_.multiply(
            np.broadcast_to(idf, csc_embeddings_.shape)
        ) * (k1 + 1)
        results = (numer / denom).sum(1).A1
        if self.normalize:
            results /= results.max()
        return results


def read_org_file(filename: Path):
    document = orgparse.load(filename).get_body(format="plain")
    return document


def get_stopwords() -> list:
    """Get a list of stopwords from STOPWORDS file."""
    with open(SCRIPT_PATH / STOPWORDS_FILENAME, "r") as f:
        stopwords = f.read().split("\n")
    return stopwords


def get_junkchars() -> list:
    """Get a list of junk characters from JUNKCHARS file."""
    with open(SCRIPT_PATH / JUNKCHARS_FILENAME, "r") as f:
        junkchars = f.read().split("\n")
        # remove empty element, which will probably exist in the file
        junkchars = [char for char in junkchars if char]
    return junkchars


def get_relative_path(source: Path, target: Path) -> Path:
    """Get the relative path to target from a source.

    Args:
        source (Path): path to the reference filename.
        target (Path): path to the target.

    Returns:
        A Path object in relative path format.
    """
    return Path(os.path.relpath(target, source.parent))


def format_results(
    input_path: Path,
    targets: List[Path],
    scores: List[float],
    num_results: int,
    id_links: bool,
    show_scores: bool,
    remove_first: bool
) -> List[str]:
    """Format results in an org-compatible format with links.

    Args:
        input_filename (Path): path to the filename that will be used as
            reference.
        target_filenames (Path): Glob containing the path to the documents
            whose similarity with the input filename will be estimated.
        scores (array-like): List of similarity scores with the same number of
            documents in target_filenames plus one (accounting for the
            input_filename).
        num_results (int): How many similar entries to list at the end of the buffer.
        id_links (bool): Whether the resulting list of similar documents will
            point to ID property or filename. Recommend setting it to True
            if you use `org-roam' v2.
        show_scores (bool): Whether to prepend the results with the similarity score.
        remove_first (bool): Remove first result from the scores list. Useful if
            the source document is inside the same directory as the target documents,
            and you don't want to see it included in the list for obvious reasons.
            Default is False.

    Returns:
        List of org formatted links to the most similar documents, sorted in descending
        order of similarity.
    """
    remove_first = int(remove_first)
    results = zip(scores, targets)
    sorted_results = sorted(results, key=lambda x: x[0], reverse=True)
    valid_results = sorted_results[remove_first:num_results+remove_first]
    formatted_results = []
    for score, target in valid_results:
        org_content = orgparse.load(target)
        title = org_content.get_file_property("title")
        score_output = f"{score:.3f} " if show_scores else ""
        if id_links:
            target_id = org_content.get_property("ID")
            link_ref = f"id:{target_id}"
        else:
            # org-mode links use relative rather than absolute paths
            target_rel_path = get_relative_path(
                source=input_path, target=target
            )
            link_ref = f"file:{target_rel_path}"
        entry = f"{score_output}[[{link_ref}][{title}]]"
        formatted_results.append(entry)
    return formatted_results


def parse_args():
    """Parse command line arguments."""
    p = argparse.ArgumentParser()
    p.add_argument(
        "--input_filename",
        "-i",
        type=str,
        help="input filename",
        required=True,
    )
    p.add_argument(
        "--directory",
        "-d",
        type=str,
        help="directory of files to search",
        required=True,
    )
    p.add_argument(
        "--algorithm",
        "-a",
        type=str,
        help="algorithm for creating the embeddings",
        required=True,
    )
    p.add_argument(
        "--number",
        "-n",
        type=int,
        default=10,
        help="number of similar documents (default: 10)",
        required=False,
    )
    p.add_argument(
        "--language",
        "-l",
        type=str,
        default="english",
        help="nltk's SnowballStemmer language (default: english)",
        required=False,
    )
    p.add_argument(
        "--scores",
        "-s",
        action="store_true",
        help="show cosine similarity scores (default: False)",
        required=False,
    )
    p.add_argument(
        "--recursive",
        "-r",
        help="search for org files recursively (default: False)",
        action="store_true",
        required=False,
    ),
    p.add_argument(
        "--id-links",
        "-I",
        action="store_true",
        help="create ID links instead of FILE links (default: False)",
        required=False,
    )
    p.add_argument(
        "--remove-first",
        "-f",
        action="store_true",
        help="remove first row from results (default: False)",
        required=False,
    )
    return p.parse_args()


def main():
    """Execute main function."""
    args = parse_args()
    input_path = Path(args.input_filename)
    directory = Path(args.directory)
    num_results = args.number
    language = args.language
    show_scores = args.scores
    recursive = args.recursive
    id_links = args.id_links
    remove_first = args.remove_first

    documents_glob = (
        directory.rglob(FILES_EXT) if recursive else directory.glob(FILES_EXT)
    )
    documents_paths = [f for f in documents_glob]
    source_content = read_org_file(input_path)
    documents_content = [read_org_file(p) for p in documents_paths]

    stemmer = SnowballStemmer(language).stem
    junkchars = get_junkchars()
    stopwords = get_stopwords()
    processor = Processor(
        junkchars=junkchars,
        stopwords=stopwords,
        stemmer=stemmer,
        lemmatize=False,
    )

    if args.algorithm == "bm25":
        model = BM25(processor=processor)
    else:
        model = Tfidf(processor=processor)

    # Add source content to list of possible words to avoid zero divisions.
    model.fit(documents_content + [source_content])
    scores = model.get_scores(source=source_content)

    formatted_results = format_results(
        input_path=input_path,
        targets=documents_paths,
        scores=scores,
        num_results=num_results,
        show_scores=show_scores,
        id_links=id_links,
        remove_first=remove_first
    )

    for entry in formatted_results:
        print(entry)


if __name__ == "__main__":
    main()
