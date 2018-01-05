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



# extract info from exercise configuration
config = json.load(sys.stdin)
home = config['resources']
source = config['source']
workdir = config['workdir']
judge = config.setdefault("prolog_judge","plunit")
time_limit = int(config['time_limit'])
memory_limit = int(config['memory_limit'])
programming_language = config['programming_language']

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

def removePath(s:str,testname):
    """Removes the path to the test file from the output
    
    Arguments:
        s {str} -- Text to clean
    
    Returns:
        str -- The cleaned text 
    """

    return  re.sub(plMountdir,"",re.sub(plTestfile,"",s)).replace("plunit_"+testname+":","")

def analyse(errorType, data, errors):
    """Adds errors to the error array
    
    Arguments:
        errorType {str} -- The tye of the error
        data {list(str)} -- a list of lines of info
        errors {list(dict)} -- list to append to
    """
    if not (errorType is None or len(data) == 0):
        n = '\n'
        d= n.join(data)
        a = {"accepted": False,"description":errorType,"messages": [{"format": "code", "description": d, "permission": "student"}]}
        errors.append(a)

def checkOutput(lines, testname):
    errorType = None
    data = []
    testcases = []
    
    for line in lines:
        line = removePath(line.rstrip(),testname)
        isStatus = plStatus.match(line)
        if isStatus:
            analyse(errorType,data,testcases)
            data = []
        else:
            isResult = plResult.match(line)
            isInfo   = plInfo.match(line)
            if isInfo:
                data.append(isInfo.group(2))
            elif isResult:
                analyse(errorType,data,testcases)
                moreinfo = isResult.group(2).strip()
                if len(moreinfo) > 0:
                    data = [moreinfo]
                else:
                    data = []
                errorType = isResult.group(1)
            else:
                data.append(line)
    analyse(errorType,data,testcases)
    return testcases

def doTest(filename, testname, comments):
    try:
        a = run(['swipl', '-s', filename, '-q', '-t', 'run_tests', '+tty','--nosignals'],timeout=5,check=False,stderr=PIPE,cwd=workdir)
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

def plunitTest(filename):
    lines = []
    initlines = [':- consult("'+source+'").\n']
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
            ctx, numNotes = doTest(testfileName,testname,comments)
            contexts.append(ctx)
            numBad += numNotes
            testname = None
            lines = []
        elif isC:
            comments.append(isC.group(1))
            lines.append(l)
        else:
            lines.append(l)

    tabs = [{"badgeCount":numBad,"groups":contexts}]
    feedback = {"accepted":numBad == 0, "groups":tabs, "status":"correct answer" if numBad == 0 else "wrong answer","description":"this is a test"}
    print(json.dumps(feedback,indent=2, separators=(',', ': '))) 





for f in os.listdir(home):
    if f.endswith(".plunit"):
        plunitTest(os.path.join(home,f))
