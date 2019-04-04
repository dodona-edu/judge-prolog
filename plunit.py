#!/usr/bin/python3
# Name: Extract PLUnit results
# By Robbert Gurdeep Singh
################################################################################
"""
PLUnit tests

http://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/plunit.html%27)
"""


import fileinput
import re
from prologGeneral import checkErrors, swipl, CondFormatString


plUnitInfo = {
    "nl": CondFormatString(
        lambda **d: d["failed"] > 0,
        """**PLUnit** voerde **{numtests} testen** uit, **{failed}** faalden. Een test kan meerdere `assert`s bevatten.""",
        "Alle **PLUnit** testen slaagden."),
    "en": CondFormatString(
        lambda **d: d["failed"] > 0,
        """**PLUnit** ran  **{numtests} tests**, **{failed}** of them failed.""",
        "All **PLUnit** tests passed.")
}


testfileName = '/tmp/tmp-testfile.pl'

plTestfile = re.compile(testfileName.replace(".", "\\.") + r"(:[0-9]*)?:?")
plMountdir = re.compile(r"/mnt/[^/]*/")
plStatus = re.compile(r"^[A.!+-]+$")
plResult = re.compile(r"^(ERROR|Warning): (.*)")
plDone = re.compile(r"done$")
plInfo = re.compile(r"^(ERROR:     |\t)(.*)")
plBeginTest = re.compile(r":- +begin_tests\(([^,]*)(,.*)?\)")
plEndTest = re.compile(r":- +end_tests\((.*)\)")
plComment = re.compile(r"%!(.*)")


class PLUnit(object):
    """Executes PLUnit code"""

    def __init__(self, config, filename, tabname="QuickCheck"):
        self.config = config
        self.filename = filename
        self.testfile = filename+"-slice.pl"
        self.lang = config["natural_language"]
        self.tabname = tabname
        self.result = None

    def getResult(self):
        if self.result is None:
            self.result = self._doTest()
        return self.result

    def getAnnotations(self):
        return None

    def getSummary(self):
        res = self.getResult()
        if res["badgeCount"] == 0:
            return "correct"
        else:
            return "PLUnit: {} issues".format(res["badgeCount"])

    def _doTest(self):
        """ Splits up the testfile in smaler parts and execute each of them.

        The file needs to be split up in order to have rusults of tests that occur
        after a non-terminating one
        """

        lines = []
        initlines = [
            ':- style_check(-singleton).\n',
            ':- style_check(-discontiguous).\n'
            '\n:- consult("{}").\n'.format(self.config["source"])
        ]
        testname = None
        comments = []

        contexts = []
        numBad = 0
        numTests = 0

        # Go over the file,
        # At :-end_tests the test is executed
        for l in fileinput.input(self.filename):
            isStart = plBeginTest.match(l)
            isEnd = plEndTest.match(l)
            isComment = plComment.match(l)

            if isStart:
                testname = isStart.group(1)
                initlines += [l for l in lines if l.strip()]
                lines = [l]
                comments = []

            elif isEnd:
                lines.append(l)

                # Create test file
                with open(self.testfile, 'w') as out:
                    out.writelines(initlines)
                    out.writelines(lines)

                ctx = self.doRun(self.testfile, testname, comments, lines)
                contexts.append(ctx)

                numTests += 1
                numBad += int(not ctx["accepted"])

                testname = None
                lines = []

            elif isComment:
                comments.append(isComment.group(1))
                lines.append(l)

            else:
                lines.append(l)

        return {
            "accepted": numBad == 0,
            "badgeCount": numBad,
            "description": self.tabname,
            "messages": [{
                "format": "markdown",
                "description": plUnitInfo[self.lang].format(
                    numtests=numTests,
                    failed=numBad)
            }],
            "groups": contexts
        }

    def doRun(self, filename, testname, comments, code):

        def oh(stdout, stderr, testname, scriptfile, config, timeout):
            """Output handler"""
            failedTests = []
            if timeout:
                failedTests.append({
                    "accepted": False,
                    "description": "Timeout " + testname,
                    "messages": [{"format": "code", "description": "The test timed out!\n\nstdOut:\n" + ("".join(stdout))}]
                })

            failedTests += checkErrors(stderr, testname)
            failedTests += checkErrors(stdout, testname)

            return failedTests

        failedTests = swipl(
            scriptfile=filename,
            testname=testname,
            goal="run_tests",
            outputHandler=oh,
            timeout=1,
            config=self.config
        )

        messages = [{"format": "plain", "description": c} for c in comments]
        messages.append({
            "format": "prolog",
            "description": "".join(code[1:-1])
        })
        if failedTests:
            context = {
                "accepted": False,
                "description": {"format": "plain", "description": testname + ": Failed"},
                "messages": messages,
                "groups": failedTests
            }
        else:
            context = {
                "accepted": True,
                "description": {"format": "plain", "description": testname + ": Passed"},
                "messages": messages,
                "groups": [{"accepted": True, "description": "Ok"}]
            }

        return context
