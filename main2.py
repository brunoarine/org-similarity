#!/usr/bin/env python3

from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.metrics.pairwise import cosine_similarity
import glob

JUNKCHARS = "junkchars.txt"
STOPWORDS = "stopwords.txt"
stopwords = open(STOPWORDS, "r").read().split("\n")
stopwords = [word for word in stopwords if word]

junkchars = open(JUNKCHARS, "r").read().split("\n")
# remove empty element, which will probably exist in the file
junkchars = [char for char in junkchars if char]

directory = "/home/soldeace/Sync/textos/zettelkasten/*.org"
input_filename = "/home/soldeace/Sync/textos/zettelkasten/20201119153159.org"

def clear_junk(text):
    for junk in junkchars:
        text = text.replace(junk, " ")
    return text

base_document = clear_junk(open(input_filename, "r").read().lower())
filenames = [filename for filename in glob.iglob(directory)]
documents = [clear_junk(open(filename, "r").read().lower()) for filename in filenames]

#base_document = "This is an example sentence for the document to be compared"
#documents = ["This is the collection of documents to be compared against the base_document"]

def process_tfidf_similarity():
    vectorizer = TfidfVectorizer(stop_words="english")

    # To make uniformed vectors, both documents need to be combined first.
    documents.insert(0, base_document)
    embeddings = vectorizer.fit_transform(documents)

    cosine_similarities = cosine_similarity(embeddings[0:1], embeddings[1:]).flatten()

    cosine_similarities = list(enumerate(cosine_similarities))
    cosine_similarities = sorted(cosine_similarities, key=lambda x:x[1], reverse=True)
    #print(cosine_similarities)

    for pair in cosine_similarities[:10]:
        i = pair[0]
        score = pair[1]
        print(score, filenames[i])


process_tfidf_similarity()
