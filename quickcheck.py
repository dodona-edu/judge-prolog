import fileinput
import re
import os
import sys
import json
import subprocess


def checkFailOutput(lines):
    """ Simple parser to parse
    -DODONA-FAIL-
    -DODONA-TEST-
    prop_reverse_twice/1
    -DODONA-COUNTEREXAMPLE-
    [0]:list(integer)
    -DODONA-END-

    lines is iterator of lines
    trows assertion error if lines are not correct
    """
    assert next(lines) == b'-DODONA-TEST-\n'
    testname = next(lines).decode("utf-8")
    assert next(lines) == b'-DODONA-COUNTEREXAMPLE-\n'

    line = next(lines)
    counterexample = b''
    while line != b'-DODONA-END-\n':
        counterexample += line
        line = next(lines)
    counterexample = counterexample.decode("utf-8")
    return {"accepted": False, "description": "Counter example for " + testname.strip(), "messages": [{"format": "code", "description": counterexample.rstrip(), "permission": "student"}]}


def checkSuccesOutput(lines):
    """ Simple parser to parse
    -DODONA-PASS-
    -DODONA-TEST-
    prop_reverse_twice/1
    -DODONA-NUMTESTS-
    100
    -DODONA-END-

    lines is iterator of lines
    trows assertion error if lines are not correct
    """
    assert next(lines) == b'-DODONA-TEST-\n'
    testname = next(lines).decode("utf-8")
    assert next(lines) == b'-DODONA-NUMTESTS-\n'
    numtests = next(lines).decode("utf-8")
    assert next(lines) == b'-DODONA-END-\n'
    return {"accepted": True, "description": "Passed " + testname.strip(), "messages": [{"format": "plain", "description": "Passed {} tests".format(numtests.strip()), "permission": "student"}]}


def checkOutput(out, err, testname):
    lines = iter(out.readlines())
    try:
        for i in range(0, 250):
            line = next(lines)
            if line == b'-DODONA-FAIL-\n':
                return checkFailOutput(lines)
            if line == b'-DODONA-PASS-\n':
                return checkSuccesOutput(lines)
        else:
            return {"accepted": False, "description": "Too much output", "messages": [{"format": "text", "description": "No usefull information in the first 250 lines of output", "permission": "student"}]}
    except StopIteration:
        return {"accepted": False, "description": "Too much output", "messages": [{"format": "text", "description": "No usefull information in the first 2500 bytes of output", "permission": "student"}]}
    except AssertionError:
        return {"accepted": False, "description": "Could not parse results", "messages": [{"format": "text", "description": "The output of our tests were badly formated, please contact the assistent.", "permission": "student"}]}


def doTest(filename, properties, comments, workdir):
    numBad = 0
    testcases = []
    for testname in properties:
        a = subprocess.Popen(
            ['swipl', '-s', filename, '-q',
                '-t', "quickcheck({})".format(testname),
                '-f', '/home/beardhatcode/Documents/lp/judge/quicktest/quickcheck.pl',
                '+tty',
                '--nosignals'],
            stderr=subprocess.PIPE,
            stdout=subprocess.PIPE,
            bufsize=2500,
            cwd=workdir)
        try:
            a.wait(timeout=1)

            testcases.append(checkOutput(a.stdout, a.stderr, testname))
            numBad = len(testcases)
        except subprocess.TimeoutExpired:
            a.terminate()
            print(a)
            print(a.stderr.read(2500).decode("utf-8"))
            a.stderr.close()

            testcases = [{"accepted": False, "description": "timeout"}]
            numBad = 1
        messages = [{"format": "plain", "description": c,
                     "permission": "student"} for c in comments]
        if len(testcases) == 0:
            context = {
                "accepted": True,
                "description": {"format": "plain", "description": testname, "permission": "student"},
                "messages": messages,
                "groups": [{"accepted": True, "description": "OK"}]
            }

        else:
            context = {
                "accepted": False,
                "description": {"format": "plain", "description": testname, "permission": "student"},
                "messages": messages,
                "groups": testcases
            }

        return context, numBad


reProperty = re.compile("(prop_[^(]*)\((.*)\)\s*:-")
reBody = re.compile("^\s")

reBraces = re.compile("\([^()]*\)")


def countArgs(params):
    depth = 0
    commas = 0
    for c in params:
        if c in ['(', '[']:
            depth += 1
        elif c in [')', ']']:
            depth -= 1
        elif c == ',' and depth == 0:
            commas += 1
    return commas + 1


def quikcheckTest(filename, tabname="QuickTest"):
    properties = []
    for l in fileinput.input(filename):
        isProp = reProperty.match(l)
        if isProp:
            properties.append("{}/{}".format(isProp.group(1),
                                             countArgs(isProp.group(2))))
    fileinput.close()

    ctx,numbad = doTest(filename, properties, [], "/tmp")

    return {"badgeCount": numbad, "description": "QuickCheck", "messages": {"format": "plain", "description": tabname + " - not implemented", "permission": "student"}, "groups": ctx}


if __name__ == "__main__":
    r = quikcheckTest("/home/beardhatcode/Documents/lp/judge/test/ex/evaluation/test.qc.pl", "wow")
    import json
    print(json.dumps(r,indent=2, separators=(',', ': ')))
