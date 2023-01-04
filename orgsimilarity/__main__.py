from rank_bm25 import BM25Okapi
import argparse
import functools
import os
import re
import sys
from itertools import compress
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

WORD_RE = re.compile(r"(?u)\b[a-z]{2,}\b")
ORG_FILE_PROPERTY_RE = re.compile(r"#\+\w+:")
ORG_FILE_PROPERTY_LINE_RE = re.compile(r"(?m)^#\+.*\n?")
ORG_TAGS_RE = re.compile(r":[\w:]*:")
URL_RE = re.compile(r"\S*https?:\S*")


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

    def preprocessor(self, text: str) -> str:
        """Remove fancy symbols and stopwords."""
        text = text.lower()
        text = text.translate({ord("’"): ord("'")})
        text = ORG_FILE_PROPERTY_RE.sub("", text)
        text = ORG_TAGS_RE.sub("", text)
        text = self._stopwords_re.sub("", text)
        text = URL_RE.sub("", text)
        return text

    def _tokenize(self, text: str) -> List[str]:
        """Preprocess a text and returns a list of tokens."""
        words = WORD_RE.findall(text)
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
    """Okapi BM25 wrapper.

    Args:
        processor (Processor): Processor object.
    """

    def __init__(self, processor):
        self.processor = processor

    def fit(self, documents: List[str]):
        """Fit IDF to documents X"""
        clean_docs = [self.processor.preprocessor(d) for d in documents]
        tokenized_docs = [self.processor.tokenizer(d) for d in clean_docs]
        self._model = BM25Okapi(tokenized_docs)

    def get_scores(self, source: str):
        clean_source = self.processor.preprocessor(source)
        tokenized_source = self.processor.tokenizer(clean_source)
        scores = self._model.get_scores(tokenized_source)
        return scores


class Corpus:
    """This wrapper provides easy access to a filtered corpus.

    Args:
        paths (list of Path): Documents paths.
        min_words (int): Minimum document size (in number of words) to include
            in the corpus. This number takes into account the number of words
            in the document bodies only, and doesn't include any kind of file
            properties (not even #+TITLE).

    Properties:
        documents_ (list of str): List of (un)filtered documents contents.
        paths_ (list of Path): List of (un)filtered document paths.

    """

    def __init__(
        self,
        paths: List[Path],
        min_words: int = 0,
    ):
        self.min_words = min_words

        self.documents_ = [read_org_file(p) for p in paths]
        self.paths_ = paths
        if min_words:
            self._apply_filter()

    def _apply_filter(self):
        """Apply min words filter in both documents and documents paths"""
        stripped_docs = [
            ORG_FILE_PROPERTY_LINE_RE.sub("", d) for d in self.documents_
        ]
        raw_tokens = [WORD_RE.findall(d) for d in stripped_docs]
        tokens_count = np.array([len(t) for t in raw_tokens])
        mask = (tokens_count > self.min_words).tolist()
        self.documents_ = list(compress(self.documents_, mask))
        self.paths_ = list(compress(self.paths_, mask))


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
    remove_first: bool,
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
    valid_results = sorted_results[remove_first : num_results + remove_first]
    formatted_results = []
    for score, target in valid_results:
        org_content = orgparse.load(target)
        title = org_content.get_file_property("title")
        score_output = f"{score:.2f} " if show_scores else ""
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
        "--min-words",
        "-m",
        type=int,
        default=0,
        help="minimum document size (in number of words) to be included in the corpus",
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
    min_words = args.min_words

    documents_glob = (
        directory.rglob(FILES_EXT) if recursive else directory.glob(FILES_EXT)
    )
    documents_paths = [f for f in documents_glob]
    corpus = Corpus(paths=documents_paths, min_words=min_words)
    source_content = read_org_file(input_path)

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
    model.fit(corpus.documents_ + [source_content])
    scores = model.get_scores(source=source_content)

    formatted_results = format_results(
        input_path=input_path,
        targets=corpus.paths_,
        scores=scores,
        num_results=num_results,
        show_scores=show_scores,
        id_links=id_links,
        remove_first=remove_first,
    )

    for entry in formatted_results:
        print(entry)


if __name__ == "__main__":
    main()
