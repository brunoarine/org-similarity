import pkg_resources
from pathlib import Path

requirements_file = Path(__file__).parent / 'requirements.txt'
pkg_resources.require(open(requirements_file, mode='r'))
pkg_resources.require("org-similarity")