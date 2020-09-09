#!/usr/bin/python3
# Name: Extract PLUnit results
# By Robbert Gurdeep Singh
##########################################################################
"""
The runner called by run,

Finds testfiles and executes them in the correct subjudge
Also executes the formatcheck for linting

"""

import os
import sys
import json
import itertools
from plunit import PLUnit
from quickcheck import QuickCheck
from simpltest import SimpleTest
from formcheck import FormCheck


# extract info from exercise configuration and set defaults
config = json.load(sys.stdin)
home = config['resources']
source = config['source']
workdir = config['workdir']
judge = config['judge']
time_limit = int(config['time_limit'])
memory_limit = int(config['memory_limit'])
programming_language = config['programming_language']

config.setdefault("prolog_stack_limit", "256m")

# Fallback for natural lanugage
if config.get("natural_language", None) not in ["en", "nl"]:
    config["natural_language"] = "en"


# Collect tests to perform
# Always FormCheck, add others if the correct files exist
tests = [FormCheck(config)]
for f in os.listdir(home):
    test = None
    fullPath = os.path.join(home, f)
    if f.endswith(".unit.pl"):
        test = PLUnit(config, fullPath, f.replace(".unit.pl", ""))
    elif f.endswith(".qc.pl"):
        test = QuickCheck(config, fullPath, f.replace(".qc.pl", ""))
    elif f.endswith(".simple.pl"):
        test = SimpleTest(config, fullPath, f.replace(".simple.pl", ""))

    if test is not None:
        tests.append(test)


tabs = [t.getResult() for t in tests if t.getResult() is not None]
annotations = list(itertools.chain.from_iterable(
    [t.getAnnotations() for t in tests if t.getAnnotations() is not None]))
numBad = sum([t["badgeCount"] for t in tabs])
accepted = all([t["accepted"] for t in tabs])

description = "issues({}).".format(numBad)

messages = []

annotationDescription = None
annotationCount = {"error": 0, "warning": 0, "info": 0}
if annotations:
    for a in annotations:
        annotationCount[a["type"]] += 1

    for t in ["error", "warning", "info"]:
        if annotationCount[t] > 0:
            annotationDescription = "{}s({}).".format(t, annotationCount[t])
            break

if accepted:
    description = "true."
    status = "correct"

    if annotationDescription:
        description = annotationDescription
        if annotationCount["error"] > 0:
            status = "compilation error"
            accepted = False

else:
    if numBad == 0:
        description = "error."
        status = "runtime error"
    else:
        status = "wrong"
    if annotationCount["error"] > 0:
        status = "compilation error"


feedback = {
    "accepted": accepted,
    "groups": tabs,
    "status": status,
    "messages": messages,
    "description": description,
    "annotations": annotations
}
print(json.dumps(feedback, indent=2, separators=(',', ': ')))
