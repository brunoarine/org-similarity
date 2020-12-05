import argparse
import glob
import os
import re
import sys

from nltk.stem import SnowballStemmer
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.metrics.pairwise import cosine_similarity


STOPWORDS = "stopwords.txt"
JUNKCHARS = "junkchars.txt"


def cmdline_args():
    """ Make parser object """
    p = argparse.ArgumentParser()
    p.add_argument(
        "--input_filename",
        "-i",
        type=str,
        help="input filename",
        required=True
    )
    p.add_argument(
        "--directory",
        "-d",
        type=str,
        help="directory of files to search",
        required=True
    )
    p.add_argument(
        "--number",
        "-n",
        type=int,
        default=10,
        help="number of similar documents (default: 10)",
        required=False
    )
    p.add_argument(
        "--language",
        "-l",
        type=str,
        default="english",
        help="nltk's SnowballStemmer language (default: english)",
        required=False
    )
    p.add_argument(
        "--score",
        "-s",
        type=bool,
        default=False,
        nargs="?",
        help="show cosine similarity score (default: False)",
        required=False
    )
    return p.parse_args()


def read_file(filename):
    """ Safely reads a filename and returns its content."""
    with open(filename, "r") as open_file:
        return open_file.read()


args = cmdline_args()
input_filename = args.input_filename
directory = args.directory
number_of_documents = args.number
language = args.language
show_score = args.score


# TODO add command line option to switch language
stemmer = SnowballStemmer(language).stem

# Fetches filenames in the same directory of the script
stopwords = open(os.path.join(sys.path[0], STOPWORDS), "r").read().split("\n")
junkchars = open(os.path.join(sys.path[0], JUNKCHARS), "r").read().split("\n")

# remove empty element, which will probably exist in the file
junkchars = [char for char in junkchars if char]


def get_tokens(text):
    """
    Preprocess a text and returns a list of tokens.
    """
    # lowercases and replaces the stupid apostrophe with a normal one
    # this step is needed because there's an Emacs package for posh people
    # that transforms every punctuation into fancy unicode symbols, and
    # this screws with the stopwords filter some lines below.
    text = text.lower()
    text = text.replace("’", "'")
    text = re.sub(r"file:\S+\]\[", "", text)  # strip org links
    text = re.sub(r"http\S+\]\[", "", text)  # strip web links
    # removes org-mode front matter and other special characters
    # that could leak into the tokens list
    for junkchar in junkchars:
        text = text.replace(junkchar, "")
    text = text.replace("\n", " ")  # replace carriage returns with a space
    # I never understood why people import scikit's tokenizer function
    # for something so dead simple?
    tokens = text.split(" ")
    tokens = [w for w in tokens if not w in stopwords]  # strip stopwords
    # replace tokens with their stems.
    # I tried using a lemmatizer like nltk's WordNet and spacy, but they
    # didn't cause much difference in the final results that could justify
    # a threefold increase in processing time.
    tokens = [stemmer(w) for w in tokens]
    return tokens


def tfidf_similarity(input_filename, target_filenames):
    """Scans the variable 'directory' for org files and uses the Scikit-learn's
    creates a sparse matrix with all found tokens via tf-idf algorithm (short
    for term frequency-inverse document frequency), which is penalizes words
    that appear too often in a text.

    base document with every other file in the directory"""

    base_document = read_file(input_filename)
    documents = [read_file(filename) for filename in target_filenames]

    vectorizer = TfidfVectorizer(tokenizer=get_tokens)

    # To make uniformed vectors, both documents need to be combined first.
    documents.insert(0, base_document)
    embeddings = vectorizer.fit_transform(documents)

    cosine_similarities = cosine_similarity(embeddings[0:1], embeddings[1:]).flatten()
    cosine_similarities = list(enumerate(cosine_similarities))
    cosine_similarities = sorted(cosine_similarities, key=lambda x: x[1], reverse=True)
    return cosine_similarities



def main():
    """Main function"""

    target_filenames = [filename for filename in glob.iglob(directory + "*.org")]

    cosine_similarities = tfidf_similarity(input_filename, target_filenames)

    for pair in cosine_similarities[1:number_of_documents+1]:
        i = pair[0]
        similar_score = pair[1]
        similar_title = open(target_filenames[i], "r").readline()
        similar_title = similar_title.replace("#+TITLE: ", "")[:-1]  # strip \n
        # org-mode links use relative rather than absolute paths
        # similar_filename = target_filenames[i].replace(directory, "")
        similar_filename = os.path.relpath(target_filenames[i],
                                           os.path.dirname(input_filename))
        if show_score:
            message = "{:.2f} [[file:{}][{}]]".format(similar_score,
                                                      similar_filename,
                                                      similar_title)
        else:
            message = "[[file:{}][{}]]".format(similar_filename,
                                               similar_title)
        print(message)


if __name__ == "__main__":
    main()
