#!/usr/bin/env python3

from setuptools import setup

setup(
    name="org-similarity",
    version="0.2",
    author="Bruno Arine",
    author_email="bruno.arine@runbox.com",
    packages=["org_similarity"],
    url="http://www.github.com/brunoarine/org-similarity",
    license="LICENSE.txt",
    description="org-similarity is a package to help Emacs org-mode users"
    "discover similar or related file",
    long_description=open("README.org").read(),
    install_requires=[
        "nltk == 3.8",
        "numpy == 1.24.0",
        "scikit-learn == 1.2.0",
        "orgparse == 0.3.1",
    ],
)
