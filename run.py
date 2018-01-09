#!/usr/bin/python3
# Name: Extract PLUnit results
# By Robbert Gurdeep Singh
################################################################################
import re
import os
import sys
import json
from plunit import PLUnit
from quickcheck import QuickCheck

words = {
    "en" : {"correct":"correct"}
}


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

tests = []
for f in os.listdir(home):
    test = None
    if f.endswith(".unit.pl"):
        test = PLUnit(config, os.path.join(home, f), f.replace(".unit.pl", ""))
    elif f.endswith(".qc.pl"):
        test = QuickCheck(config, os.path.join(home, f), f.replace(".qc.pl", ""))
    else:
        continue
    tests.append(test)

tabs = [t.getResult() for t in tests]
numBad = sum([t["badgeCount"] for t in tabs])
accepted = all([t["badgeCount"] == 0 for t in tabs])
feedback = {
    "accepted": accepted, 
    "groups": tabs, 
    "status": "correct answer" if numBad == 0 else "wrong answer", 
    "description": "issues({}).".format(numBad) if numBad > 0 else "true."
    }
print(json.dumps(feedback, indent=2, separators=(',', ': ')))