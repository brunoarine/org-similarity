#!/usr/bin/env python3

import sys
import os
import argparse
from nltk.stem import LancasterStemmer
import glob
import numpy as np
import re

#directory = "/home/soldeace/Sync/textos/zettelkasten/"
#input_filename = "/home/soldeace/Sync/textos/zettelkasten/20201119153159.org"

stemmer = LancasterStemmer().stem

STOPWORDS = "stopwords.txt"
JUNKCHARS = "junkchars.txt"

stopwords = open(os.path.join(sys.path[0], STOPWORDS), "r").read().split("\n")
junkchars = open(os.path.join(sys.path[0], JUNKCHARS), "r").read().split("\n")
# remove empty element, which will probably exist in the file
junkchars = [char for char in junkchars if char]


def read_file(filename):
    """
    Reads a text file and cleans it.
    """
    with open(filename, "r") as open_file:
        text = open_file.read()
    text = text.lower().replace("\n", " ")
    text = re.sub(r"file:\S+\]\[", "", text)
    text = re.sub(r"http\S+\]\[", "", text)
    for junk in junkchars:
        text = text.replace(junk, " ")
    text = text.split(" ")
    text = [word for word in text if word not in stopwords]
    text = [word for word in text if word]
    text = [stemmer(word) for word in text]
    return text



def similarity(input_text, target_text):
    """
    Calculates a 'soft' cosine similarity between two texts. The word vector is
    made of the input text only.
    """
    unique_words = set(input_text+target_text)
    sum_input_prod_target = 0.0
    sum_input = 0.0
    sum_target = 0.0
    for word in unique_words:
        sum_input_prod_target += input_text.count(word)*target_text.count(word)
        sum_input += input_text.count(word)**2
        sum_target += target_text.count(word)**2
        # print(word,sum_input,sum_target)
    if sum_input*sum_target > 0:
        return sum_input_prod_target/(sum_input**0.5*sum_target**0.5)
    else:
        return 0

def cmdline_args():
        # Make parser object
    p = argparse.ArgumentParser()

    p.add_argument("--input_file", "-i", type=str, help="input filename", required=True)
    p.add_argument("--directory", "-d", type=str, help="directory of files to search", required=True)

    return(p.parse_args())


def main():
    args = cmdline_args()

    input_file = args.input_file
    directory = args.directory

    input_text = read_file(input_file)
    target_files = glob.iglob(directory + "*.org")

    similarity_values = []
    filenames = []
    for filename in target_files:
       target_text = read_file(filename)
       filenames.append(filename)
       similarity_values.append(similarity(input_text, target_text))

       ranking = zip(similarity_values, filenames)
       ranking = sorted(ranking, key=lambda t: t[0], reverse=True)

    for entry in ranking[1:10]:
        similar_number = entry[0]
        similar_path = entry[1]
        similar_title = open(similar_path, "r").readline().replace("#+TITLE: ", "").replace("\n","")
        similar_filename = similar_path.replace(directory, "")
        message = str(round(similar_number,2)) + " [[file:{}][{}]]".format(similar_filename, similar_title)
        print(message)


if __name__ == "__main__":
    main()
