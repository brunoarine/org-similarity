import numpy as np
from pathlib import Path
from orgsimilarity.__main__ import (
    Processor,
    get_junkchars,
    get_stopwords,
    Tfidf,
    BM25,
)
from sklearn.feature_extraction.text import TfidfVectorizer
from nltk.stem import SnowballStemmer, PorterStemmer
from tqdm import tqdm
import argparse
from scipy.stats import spearmanr


def parse_args():
    """Parse command line arguments."""
    p = argparse.ArgumentParser()
    p.add_argument(
        "--input",
        "-i",
        type=str,
        help="input filename",
        required=True,
    )
    p.add_argument(
        "--verbose",
        "-v",
        action="store_true",
        help="increase verbosity",
        required=False,
    )
    return p.parse_args()


def load_data(filename: Path):
    scores = []
    sentences_a = []
    sentences_b = []
    with open(filename, "r") as f:
        data = f.readlines()
    for row in data:
        if len(row) != 7:
            pass
        row_splitted = row.strip().split("\t")
        scores.append(float(row_splitted[4]))
        sentences_a.append(row_splitted[5])
        sentences_b.append(row_splitted[6])
    return scores, sentences_a, sentences_b


def main():
    args = parse_args()
    sample_file = Path(__file__).parent / args.input
    real_scores, sentences_a, sentences_b = load_data(sample_file)

    language = "english"
    stemmer = SnowballStemmer(language).stem
    junkchars = get_junkchars()
    stopwords = get_stopwords()
    processor = Processor(
        junkchars=junkchars,
        stopwords=stopwords,
        stemmer=stemmer,
        lemmatize=True,
    )
    model = Tfidf(processor=processor, sublinear_tf=True)
    # model = BM25(processor=processor, normalize=True)
    model.fit(sentences_b + sentences_a)

    predicted_scores = []
    for idx, a in tqdm(enumerate(sentences_a), total=len(sentences_a)):
        scores = model.get_scores(source=a)
        if np.isnan(scores[idx]):
            print(f"NaN found! idx: {idx}\nA: {a}\nB: {sentences_b[idx]}")
        predicted_scores.append(scores[idx])

    if args.verbose:
        for a, b, real, predicted in zip(
            sentences_a, sentences_b, real_scores, predicted_scores
        ):
            print(f"A: {a}\tB: {b}\t{real}\t{predicted:.4f}")

    predicted_scores = np.array(predicted_scores)
    real_scores = np.array(real_scores)
    correlation = spearmanr(predicted_scores, real_scores).correlation
    print("Correlation:", correlation)


if __name__ == "__main__":
    main()
