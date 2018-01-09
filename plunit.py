#!/usr/bin/python3
# Name: Extract PLUnit results
# By Robbert Gurdeep Singh
################################################################################
import fileinput
import re
import os
import sys
import json
from subprocess import PIPE, TimeoutExpired, run
from prologGeneral import checkErrors, swipl


plUnitInfo = {
    "nl": """**PLUnit** voerde **{numtests} testen** uit, **{failed}** faalden. 

Hieronder zie je de output van PLUnit.
""",
    "en": """**PLUnit** ran  **{numtests} tests**, **{failed}** of them failed. 

The output of PLUnit is shown below
"""
}


testfileName = '/tmp/tmp-testfile.pl'

plTestfile = re.compile(testfileName.replace(".", "\\.") + "(:[0-9]*)?:?")
plMountdir = re.compile("/mnt/[^/]*/")
plStatus = re.compile("^[A.!+-]+$")
plResult = re.compile("^(ERROR|Warning): (.*)")
plDone = re.compile("done$")
plInfo = re.compile("^(ERROR:     |\t)(.*)")
plBeginTest = re.compile(":- +begin_tests\(([^,]*)(,.*)?\)")
plEndTest = re.compile(":- +end_tests\((.*)\)")
plComment = re.compile("%!(.*)")


def doTest(filename, testname, comments, config):

    def oh(stdout, stderr, testname, scriptfile, config, timeout):
        testcases = []
        if timeout:
            testcases.append({
                "accepted": False,
                "description": "Timeout " + testname,
                "messages": [{"format": "code", "description": "The test timed out!\n\nstdOut:\n" + ("".join(stdout))}]
            })

        testcases += checkErrors(stderr, testname)

        return testcases

    testcases = swipl(
        scriptfile=filename,
        testname=testname,
        goal="run_tests",
        outputHandler=oh,
        timeout=5,
        config=config
    )

    messages = [{"format": "plain", "description": c} for c in comments]
    if len(testcases) == 0:
        context = {
            "accepted": True,
            "description": {"format": "plain", "description": testname},
            "messages": messages,
            "groups": [{"accepted": True, "description": "Ok"}]
        }

    else:
        context = {
            "accepted": False,
            "description": {"format": "plain", "description": testname},
            "messages": messages,
            "groups": testcases
        }

    return context


def plunitTest(config, filename, tabname="PLUnit"):
    lines = []
    initlines = ['\n:- consult("{}").\n'.format(config["source"])]
    testname = None
    comments = []

    contexts = []
    numBad = 0
    numTests = 0

    for l in fileinput.input(filename):
        isS = plBeginTest.match(l)
        isE = plEndTest.match(l)
        isC = plComment.match(l)
        if isS:
            testname = isS.group(1)
            if len(lines) > 0:
                initlines += [l for l in lines if len(l.strip()) > 0]
            lines = [l]
            comments = []
        elif isE:
            lines.append(l)
            with open(testfileName, 'w') as out:
                out.writelines(initlines)
                out.writelines(lines)
            ctx = doTest(testfileName, testname, comments, config)
            numTests += 1
            contexts.append(ctx)
            numBad += int(not ctx["accepted"])
            testname = None
            lines = []
        elif isC:
            comments.append(isC.group(1))
            lines.append(l)
        else:
            lines.append(l)

    return {
        "badgeCount": numBad,
        "description": "PLUnit",
        "messages": [{
            "format": "markdown",
            "description": plUnitInfo[config["natural_language"]].format(
                numtests=numTests, 
                failed=numBad)
        }],
        "groups": contexts
    }
