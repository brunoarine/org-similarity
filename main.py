#!/usr/bin/env python3

import glob

STOPWORDS = "stopwords.txt"
JUNKCHARS = "junkchars.txt"

directory = "/home/soldeace/Sync/textos/zettelkasten/*.org"
#directory = "texts/*.*"
#input_filename = "texts/irene.txt"
input_filename = "/home/soldeace/Sync/textos/zettelkasten/20201119153159.org"

stopwords = open(STOPWORDS, "r").read().split("\n")
junkchars = open(JUNKCHARS, "r").read().split("\n")
# remove empty element, which will probably exist in the file
junkchars = [char for char in junkchars if char]


def strip_links(text, brackets="[]"):
    count = [0] * (len(brackets) // 2)  # count open/close brackets
    saved_chars = []
    for character in text:
        for i, b in enumerate(brackets):
            if character == b:  # found bracket
                kind, is_close = divmod(i, 2)
                count[kind] += (-1)**is_close  # `+1`: open, `-1`: close
                if count[kind] < 0:  # unbalanced bracket
                    count[kind] = 0  # keep it
                else:  # found bracket to remove
                    break
        else:  # character is not a [balanced] bracket
            if not any(count):  # outside brackets
                saved_chars.append(character)
    return ''.join(saved_chars)


def read_file(filename):
    """
    Reads a text file and cleans it.
    """
    with open(filename, "r") as open_file:
        text = open_file.read()
    text = text.lower().replace("\n", " ")
    #text = strip_links(text)
    for junk in junkchars:
        text = text.replace(junk, " ")
    text = text.split(" ")
    text = [word for word in text if word not in stopwords]
    text = [word for word in text if word]
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


input_text = read_file(input_filename)
target_files = glob.iglob(directory)

simvalues = []
filenames = []
for filename in target_files:
    target_text = read_file(filename)
    filenames.append(filename)
    simvalues.append(similarity(input_text, target_text))

ranking = zip(simvalues, filenames)
ranking = sorted(ranking, key=lambda t: t[0], reverse=True)

for entry in ranking[:10]:
    print(entry[0], entry[1])
