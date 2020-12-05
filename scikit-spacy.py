import sys
import os
import argparse
from nltk.stem import SnowballStemmer
from nltk.stem import WordNetLemmatizer
import nltk
import glob
import string
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.metrics.pairwise import cosine_similarity
import re
import spacy

nlp = spacy.load('en')
#Use nlp.disable_pipes('tagger','ner') to speed up lemmatization
nlp.disable_pipes('tagger', 'ner')

STOPWORDS = "stopwords.txt"
JUNKCHARS = "junkchars.txt"

stemmer = SnowballStemmer("english").stem
#stemmer = WordNetLemmatizer().lemmatize

def tokenize(text):
    analyzed_text = nlp(text)
    lemmas = [token.lemma_ for token in analyzed_text]
    return lemmas

stopwords = open(os.path.join(sys.path[0], STOPWORDS), "r").read().split("\n")
junkchars = open(os.path.join(sys.path[0], JUNKCHARS), "r").read().split("\n")
# remove empty element, which will probably exist in the file
junkchars = [char for char in junkchars if char]


def read_file(filename):
    with open(filename, "r") as open_file:
        return open_file.read()

def get_tokens(text):
    lowers = text.lower().replace("’", "'")
    lowers = re.sub(r"file:\S+\]\[", "", lowers)
    lowers = re.sub(r"http\S+\]\[", "", lowers)
    #remove the punctuation using the character deletion step of translate
    for junkchar in junkchars:
        lowers = lowers.replace(junkchar, "")
    print(lowers)
    lowers = lowers.replace("\n"," ")
    lowers = tokenize(lowers)
    lowers = [w.replace(" ", "") for w in lowers]
    tokens = list(filter(None, lowers))
    tokens = [w for w in tokens if not w in stopwords]
    #tokens = [stemmer(w) for w in tokens]
    return tokens

def process_tfidf_similarity():
    vectorizer = TfidfVectorizer(tokenizer=get_tokens)

    # To make uniformed vectors, both documents need to be combined first.
    documents.insert(0, base_document)
    embeddings = vectorizer.fit_transform(documents)

    cosine_similarities = cosine_similarity(embeddings[0:1], embeddings[1:]).flatten()

    cosine_similarities = list(enumerate(cosine_similarities))
    cosine_similarities = sorted(cosine_similarities, key=lambda x:x[1], reverse=True)
    #print(cosine_similarities)

    for pair in cosine_similarities[1:10]:
        i = pair[0]
        score = pair[1]
        similar_filename = filenames[i]
        similar_title = open(similar_filename, "r").readline().replace("#+TITLE: ", "")[:-1]
        message = str(round(score,2)) + " [[file:{}][{}]]".format(similar_filename.replace(directory, ""), similar_title)
        print(message)

def cmdline_args():
        # Make parser object
    p = argparse.ArgumentParser()

    p.add_argument("--input_filename", "-i", type=str, help="input filename", required=True)
    p.add_argument("--directory", "-d", type=str, help="directory of files to search", required=True)

    return(p.parse_args())

# args = cmdline_args()
# input_filename = args.input_filename
# directory = args.directory

directory = "/home/soldeace/Sync/textos/zettelkasten/"
input_filename = "/home/soldeace/Sync/textos/zettelkasten/20201119153159.org"
base_document = read_file(input_filename)
filenames = [filename for filename in glob.iglob(directory + "*.org")]
documents = [read_file(filename) for filename in filenames]
process_tfidf_similarity()
