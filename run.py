#!/usr/bin/python3
# Name: Extract PLUnit results
# By Robbert Gurdeep Singh
################################################################################
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

words = {
    "en": {"notes": "There are annotations for your code in the code tab"},
    "nl": {"notes": "Er zijn annotaties voor jouw code in het code tabblad"}
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

tests = [FormCheck(config)]
for f in os.listdir(home):
    test = None
    if f.endswith(".unit.pl"):
        test = PLUnit(config, os.path.join(home, f), f.replace(".unit.pl", ""))
    elif f.endswith(".qc.pl"):
        test = QuickCheck(config, os.path.join(
            home, f), f.replace(".qc.pl", ""))
    elif f.endswith(".simple.pl"):
        test = SimpleTest(config, os.path.join(home, f),
                          f.replace(".simple.pl", ""))
    else:
        continue
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


if annotations:
    annotationKinds = ", ".join([str(annotationCount[t]) + " " + t for t in [
                                "error", "warning", "info"] if annotationCount[t] > 0])
    messages.append({
        "format": "code",
        "description": "  "+words[config["natural_language"]]["notes"] + " (" + annotationKinds + ")"
    })

feedback = {
    "accepted": accepted,
    "groups": tabs,
    "status": status,
    "messages": messages,
    "description": description,
    "annotations": annotations
}
print(json.dumps(feedback, indent=2, separators=(',', ': ')))
