import fileinput
import re
import os
import sys
import json


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


def removePath(s: str, testname):
    """Removes the path to the test file from the output

    Arguments:
        s {str} -- Text to clean

    Returns:
        str -- The cleaned text 
    """

    return re.sub(plMountdir, "", re.sub(plTestfile, "", s)).replace("plunit_" + testname + ":", "")


def analyse(errorType, data, errors):
    """Adds errors to the error array

    Arguments:
        errorType {str} -- The tye of the error
        data {list(str)} -- a list of lines of info
        errors {list(dict)} -- list to append to
    """
    if not (errorType is None or len(data) == 0):
        n = '\n'
        d = n.join(data)
        a = {"accepted": False, "description": errorType, "messages": [
            {"format": "code", "description": d, "permission": "student"}]}
        errors.append(a)


def checkErrors(lines, testname):
    errorType = None
    data = []
    testcases = []

    for line in lines:
        line = removePath(line.rstrip(), testname)
        isStatus = plStatus.match(line)
        if isStatus:
            analyse(errorType, data, testcases)
            data = []
        else:
            isResult = plResult.match(line)
            isInfo = plInfo.match(line)
            if isInfo:
                data.append(isInfo.group(2))
            elif isResult:
                analyse(errorType, data, testcases)
                moreinfo = isResult.group(2).strip()
                if len(moreinfo) > 0:
                    data = [moreinfo]
                else:
                    data = []
                errorType = isResult.group(1)
            else:
                data.append(line)
    analyse(errorType, data, testcases)
    return testcases
