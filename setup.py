#!/usr/bin/env python3

from setuptools import setup

setup(
    name="org-similarity",
    version="0.2",
    author="Bruno Arine",
    author_email="bruno.arine@runbox.com",
    packages=["orgsimilarity"],
    url="http://www.github.com/brunoarine/org-similarity",
    license="LICENSE.txt",
    description="org-similarity is a package to help Emacs org-mode users"
    "discover similar or related file",
    long_description=open("README.org").read(),
    package_data={'': ['*.txt', 'orgsimilarity/*.txt']},
    include_package_data=True,
    install_requires=[
    ],
)
