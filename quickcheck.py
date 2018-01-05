import fileinput
import re
import os 
import sys
import json
from subprocess import PIPE,TimeoutExpired,run


def doTest(filename, testname, comments,workdir):
    try:
        print(" ".join(['swipl', '-s', filename, '-q', '-t', "quickcheck({})".format(testname),'-f', '/home/beardhatcode/Documents/lp/judge/quicktest/quickcheck.pl', '+tty','--nosignals']))
        a = run(['swipl', '-s', filename, '-q', '-t', "quickcheck({})".format(testname), '+tty','--nosignals'],timeout=1,check=False,stderr=PIPE,cwd=workdir)
        output = a.stderr.decode("utf-8")
        testcases = checkOutput(output.splitlines(),testname)
        numBad = len(testcases)
    except TimeoutExpired:
        testcases = [{"accepted": False,"description":"timeout"}]
        numBad = 1
    messages = [{"format":"plain","description":c,"permission":"student"} for c in comments]
    if len(testcases) == 0:
        context = {
            "accepted":True, 
            "description":{"format":"plain","description":testname,"permission":"student"},
            "messages": messages,
            "groups":[{"accepted":True,"description":"OK"}] 
            }
            
    else:
        context = {
            "accepted":False, 
            "description":{"format":"plain","description":testname,"permission":"student"},
            "messages": messages,
            "groups":testcases
            }
    
    return context,numBad


reProperty = re.compile("(prop_[^(]*)\((.*)\)\s*:-");
reBody = re.compile("^\s");

reBraces = re.compile("\([^()]*\)");

def countArgs(params):
    depth = 0
    commas = 0
    for c in params:
        if c in ['(','[']:
            depth += 1
        elif c in [')',']']:
            depth -= 1
        elif c == ',' and depth == 0:
            commas += 1
    return commas + 1

def quikcheckTest(filename,tabname="QuickTest"):
    properties = []
    for l in fileinput.input(filename):
        isProp = reProperty.match(l)
        if isProp:
            properties.append("{}/{}".format(isProp.group(1),countArgs(isProp.group(2))));
    fileinput.close()

    print("checks to perform")
    for p in properties:
        print(doTest(filename,p,[],"/tmp"))

    return {"badgeCount":1,"description":"QuickCheck","messages":{"format":"plain","description":tabname + " - not implemented","permission":"student"},"groups":[]}

if __name__ == "__main__":
    print(quikcheckTest("/home/beardhatcode/Documents/lp/judge/test/ex/evaluation/test.qc.pl","wow"))