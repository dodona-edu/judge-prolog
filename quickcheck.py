import fileinput
import re
import os
import sys
import json
import subprocess
from prologError import checkErrors




reProperty = re.compile("(prop_[^(]*)\((.*)\)\s*:-")
reBody = re.compile("^\s")

reBraces = re.compile("\([^()]*\)")

class QuickCheck(object):
    def __init__(self,config, filename, tabname="QuickTest"):
        self.config = config
        self.tabname = tabname
        self.timeout = 1
        self.bufsize = 2500

        
        # Read input
        data = [l for l in fileinput.input(filename)]
        fileinput.close()

        # get property definitions
        self.properties = {}
        startLine  = 0
        curProperty = None
        for i,l in enumerate(data+["\n"]):
            isProp = reProperty.match(l)
            if isProp:
                startLine = i
                curProperty = "{}/{}".format(isProp.group(1), countArgs(isProp.group(2)))
            elif len(l.strip()) == 0 and curProperty is not None:
                if curProperty in self.properties:
                    self.properties[curProperty] += data[startLine:i]
                else:
                    self.properties[curProperty] = data[startLine:i]
                curProperty = None
        

        # Make a new testfile that consults the users solution
        # TODO: just make a file with multiple consults (user code and test code)
        self.testfileName = filename+".extended.pl"
        with open(self.testfileName, "w") as f2:
            f2.write(':- consult("{}").\n'.format(config["source"]))
            for line in data:
                f2.write(line)


    def doTest(self):
        totalNumBad = 0
        contexts = []
        for testname in self.properties:
            testcases = self.run(testname)
            numBad = sum([not t["accepted"] for t in testcases])
            if numBad == 0:
                context = {
                    "accepted": True,
                    "description": testname,
                    "groups": testcases
                }
            else:
                context = {
                    "accepted": False,
                    "description": testname,
                    "groups": testcases
                }
            contexts.append(context)
            totalNumBad += numBad

        return {"badgeCount": totalNumBad, "description": self.tabname, "messages": [], "groups": contexts}

    def run(self,testname):
        testcases = []
        a = subprocess.Popen(
            ['swipl', '-s', self.testfileName, '-q',
                '-t', "quickcheck({})".format(testname),
                '-f', self.config["judge"]+'/quicktest/quickcheck.pl',
                '+tty',
                '--nosignals'],
            stderr=subprocess.PIPE,
            stdout=subprocess.PIPE,
            bufsize=self.bufsize,
            cwd=self.config["workdir"])
        try:
            a.wait(timeout=self.timeout)
            testcases.append(self.checkOutput(a.stdout, testname))
            testcases += checkErrors(a.stderr.read(self.bufsize).decode("utf-8").splitlines(),testname)
        except subprocess.TimeoutExpired:
            a.terminate()
            resOut = a.stdout.read(self.bufsize).decode("utf-8")
            testcases += checkErrors(a.stderr.read(self.bufsize).decode("utf-8").splitlines(),testname)

            testcases.append({
                "accepted": False, 
                "description": "Timeout " + testname,
                "messages": [{"format": "code", "description": "stdOut:\n" + resOut, "permission": "student"}]
                })
        a.stderr.close()
        a.stdout.close()
        return testcases

    def checkFailOutput(self, lines):
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
        testname = next(lines).decode("utf-8").strip()
        assert next(lines) == b'-DODONA-COUNTEREXAMPLE-\n'

        line = next(lines)
        counterexample = b''
        while line != b'-DODONA-END-\n':
            counterexample += line
            line = next(lines)
        counterexample = counterexample.decode("utf-8")
        return {
            "accepted": False,
            "description": "Counter example",
            "messages": [{"format": "markdown", "description": "De waarde\n\n ```prolog\n{}\n```\n\n faalt\n\n```prolog\n{}```".format(counterexample.strip(),"".join(self.properties[testname])), "permission": "student"}]
        }


    def checkSuccesOutput(self,lines):
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
        testname = next(lines).decode("utf-8").strip()
        assert next(lines) == b'-DODONA-NUMTESTS-\n'
        numtests = next(lines).decode("utf-8").strip()
        assert next(lines) == b'-DODONA-END-\n'
        return {
            "accepted": True,
            "description": "{} Tests passed".format(numtests)
        }


    def checkOutput(self,out, testname):
        lines = iter(out.readlines())
        notmatched = []
        try:
            for i in range(0, 250):
                line = next(lines)
                if line == b'-DODONA-FAIL-\n':
                    return self.checkFailOutput(lines)
                if line == b'-DODONA-PASS-\n':
                    return self.checkSuccesOutput(lines)
                notmatched.append(line.decode("utf-8").strip())
        except StopIteration:
            pass
        except AssertionError:
            return {"accepted": False, "description": "Could not parse results", "messages": [{"format": "text", "description": "The output of our tests were badly formated, please contact the assistent.", "permission": "student"}]}

        return {"accepted": False, "description": "Could not execute test"}
        


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

