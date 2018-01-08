#!/usr/bin/python3
# Name: Extract PLUnit results
# By Robbert Gurdeep Singh
################################################################################
import fileinput
import re
import os 
import sys
import json
from subprocess import PIPE,TimeoutExpired,run
from prologError import checkErrors



testfileName = '/tmp/tmp-testfile.pl';

plTestfile = re.compile(testfileName.replace(".","\\.")+"(:[0-9]*)?:?")
plMountdir=re.compile("/mnt/[^/]*/")
plStatus = re.compile("^[A.!+-]+$")
plResult = re.compile("^(ERROR|Warning): (.*)")
plDone = re.compile("done$")
plInfo = re.compile("^(ERROR:     |\t)(.*)")
plBeginTest = re.compile(":- +begin_tests\(([^,]*)(,.*)?\)")
plEndTest   = re.compile(":- +end_tests\((.*)\)")
plComment   = re.compile("%!(.*)")


def doTest(filename, testname, comments,workdir):
    try:
        a = run(['swipl', '-s', filename, '-q', '-t', 'run_tests', '+tty','--nosignals'],timeout=5,check=False,stderr=PIPE,cwd=workdir)
        output = a.stderr.decode("utf-8")
        testcases = checkError(output.splitlines(),testname)
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

def plunitTest(config,filename,tabname="PLUnit"):
    lines = []
    initlines = ['\n:- consult("{}").\n'.format(config["source"])]
    testname = None
    comments = []

    contexts = []
    numBad = 0

    for l in fileinput.input(filename):
        isS = plBeginTest.match(l)
        isE = plEndTest.match(l)
        isC = plComment.match(l)
        if isS:
            testname = isS.group(1)
            if len(lines) > 0:
                initlines += [l for l in lines if len(l.strip()) >0]
            lines = [l]
            comments = []
        elif isE:
            lines.append(l)
            with open(testfileName,'w') as out:
                out.writelines(initlines)
                out.writelines(lines)
            ctx, numNotes = doTest(testfileName,testname,comments,config["workdir"])
            contexts.append(ctx)
            numBad += numNotes
            testname = None
            lines = []
        elif isC:
            comments.append(isC.group(1))
            lines.append(l)
        else:
            lines.append(l)

    return {"badgeCount":numBad,"description":"PLUnit","messages":[{"format":"plain","description":tabname,"permission":"student"}],"groups":contexts}

