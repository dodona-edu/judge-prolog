#!/usr/bin/python3
# Name: Extract PLUnit results
# By Robbert Gurdeep Singh
################################################################################
import re
import os
import sys
import json
from plunit import plunitTest
from quickcheck import QuickCheck


plMountdir = re.compile("/mnt/[^/]*/")


def removePath(s: str, testname):
    """Removes the path to the test file from the output

    Arguments:
        s {str} -- Text to clean

    Returns:
        str -- The cleaned text 
    """

    return re.sub(plMountdir, "", s)



# extract info from exercise configuration and set defaults
config = json.load(sys.stdin)
home = config['resources']
source = config['source']
workdir = config['workdir']
judge = config['judge']
time_limit = int(config['time_limit'])
memory_limit = int(config['memory_limit'])
programming_language = config['programming_language']

config.setdefault("prolog_local_stack", "128M")
config.setdefault("prolog_global_stack", "128M")
config.setdefault("prolog_trail_stack", "128M")

if ("natural_language" not in config) or (config["natural_language"] not in ["en", "nl"]):
    config["natural_language"] = "en"

tabs = []
for f in os.listdir(home):
    if f.endswith(".plunit"):
        tabs.append(plunitTest(config, os.path.join(home, f), f))
    elif f.endswith(".qc.pl"):
        qc = QuickCheck(config, os.path.join(home, f), f.replace(".qc.pl", ""))
        tabs.append(qc.doTest())

numBad = sum([t["badgeCount"] for t in tabs])
feedback = {"accepted": numBad == 0, "groups": tabs, "status": "correct answer" if numBad ==
            0 else "wrong answer", "description": str(numBad) + " errors"}
print(json.dumps(feedback, indent=2, separators=(',', ': ')))
