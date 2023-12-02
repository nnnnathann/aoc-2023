

import os
import requests
import pathlib

SESSION_KEY_FILE = "../data/session_key.txt"

def read_or_download(year, day):
    """
    Download the input file for the given year and day from the Advent of Code website.
    If the file already exists, read it from disk.
    """
    filename = "../data/" + str(year) + "/input/" + str(day) + ".txt"
    if not os.path.isfile(filename):
        print("downloading input file for day " + str(day) + "...")
        url = "http://adventofcode.com/" + str(year) + "/day/" + str(day) + "/input"
        r = requests.get(url, cookies={"session": read_file(SESSION_KEY_FILE).strip()})
        pathlib.Path(os.path.dirname(filename)).mkdir(parents=True, exist_ok=True)
        with open(filename, "w") as f:
            f.write(r.text)
    return read_file(filename).strip()

def read_file(file):
    """
    read file content
    """
    with open(file, "r") as f:
        return str(f.read())