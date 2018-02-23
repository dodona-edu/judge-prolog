import fileinput
import re
from prologGeneral import checkErrors, swipl


reProperty = re.compile(r"(prop_[^(]*)\((.*)\)\s*:-")
reBody = re.compile(r"^\s")

reBraces = re.compile(r"\([^()]*\)")

quickCheckInfo = {
    "nl": """**Quckcheck** controleerde **{numtests} predikaten** over jou code, hievan faalden er **{failed}**. 

Hieronder zie je de code die de predicaten voorstelt en als ze faalden een tegenvoorbeeld.
""",
    "en": """**Quckcheck** checked  **{numtests} predicates** over your code, **{failed}** of which failed. 

The results below show the code that represents the predicates. If they fail, a counterexample is given.
"""
}


class QuickCheck(object):
    def __init__(self, config, filename, tabname="QuickCheck"):
        self.config = config
        self.tabname = tabname
        self.timeout = 1
        self.bufsize = 2500
        self.numlines = 250
        self.lang = config["natural_language"]
        self.result = None

        # Read input
        data = [l for l in fileinput.input(filename)]
        fileinput.close()

        # get property definitions
        self.properties = {}
        self.orderedProperties = []
        startLine = 0
        curProperty = None
        for i, l in enumerate(data + ["\n"]):
            isProp = reProperty.match(l)
            if isProp:
                startLine = i
                curProperty = "{}/{}".format(isProp.group(1),
                                             countArgs(isProp.group(2)))
            elif len(l.strip()) == 0 and curProperty is not None:
                if curProperty in self.properties:
                    self.properties[curProperty] += data[startLine:i]
                else:
                    self.orderedProperties.append(curProperty)
                    self.properties[curProperty] = data[startLine:i]
                curProperty = None

        # Make a new testfile that consults the users solution
        # and the check file
        self.testfileName = filename + ".extended.pl"
        consultLine = ':- consult("{}").\n'
        with open(self.testfileName, "w") as f2:
            f2.write(consultLine.format(config["source"]))
            f2.write(consultLine.format(filename))
            f2.write(consultLine.format(
                self.config["judge"] + '/quicktest/quickcheck.pl'))

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
            return "QuickCheck: {} issues".format(res["badgeCount"])


    def _doTest(self):
        totalNumBad = 0
        contexts = []
        failedTest = 0
        for testname in self.orderedProperties:
            testcases = self._run(testname)
            numBad = sum([not t["accepted"]
                          for t in testcases if "accepted" in t])

            if numBad == 0:
                context = {
                    "accepted": True,
                    "description": testname + ": passed",
                    "groups": testcases,
                    "messages": [{
                        "format": "code",
                        "description": "".join(self.properties[testname])
                    }]
                }
            else:
                context = {
                    "accepted": False,
                    "description": testname + ": failed",
                    "groups": testcases,
                    "messages": [{
                        "format": "code",
                        "description": "".join(self.properties[testname])
                    }]
                }
                failedTest += 1

            contexts.append(context)
            totalNumBad += numBad

        return {
            "accepted" : failedTest == 0,
            "badgeCount": failedTest,
            "description": self.tabname,
            "messages": [{
                "format": "markdown",
                "description": quickCheckInfo[self.lang].format(
                    numtests=len(self.orderedProperties),
                    failed=failedTest
                )
            }],
            "groups": contexts
        }

    def _run(self, testname):
        def oh(stdout, stderr, testname, scriptfile, config, timeout):
            testcases = []
            if timeout:
                testcases.append({
                    "accepted": False,
                    "description": "Timeout " + testname,
                    "messages": [{"format": "code", "description": "The test timed out (more than 1s)!\n\nstdOut:\n" + ("".join(stdout))}]
                })
            else:
                testcases.append(self._checkOutput(stdout, testname))

            testcases += checkErrors(stderr, testname)
            return testcases

        testcases = swipl(
            scriptfile=self.testfileName,
            testname=testname,
            goal="quickcheck({})".format(testname),
            outputHandler=oh,
            timeout=1,
            config=self.config)
        return testcases

    def _checkFailOutput(self, lines):
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
        assert next(lines) == '-DODONA-TEST-\n'
        testname = next(lines).strip()
        assert next(lines) == '-DODONA-COUNTEREXAMPLE-\n'

        line = next(lines)
        counterexample = ""
        while line != "-DODONA-END-\n":
            counterexample += line
            line = next(lines)
        counterexample = counterexample.strip('\n')
        return {
            "accepted": False,
            "description": "Counter example",
            "messages": [{"format": "code", "description": counterexample}]
        }

    def _checkSuccesOutput(self, lines):
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
        assert next(lines) == "-DODONA-TEST-\n"
        testname = next(lines).strip()
        assert next(lines) == "-DODONA-NUMTESTS-\n"
        numtests = next(lines).strip()
        assert next(lines) == "-DODONA-END-\n"
        return {
            "accepted": True,
            "description": "{} Tests passed".format(numtests)
        }

    def _checkOutput(self, out, testname):
        lines = iter(out)
        notmatched = []
        try:
            while True:
                line = next(lines)
                if line == '-DODONA-FAIL-\n':
                    return self._checkFailOutput(lines)
                if line == '-DODONA-PASS-\n':
                    return self._checkSuccesOutput(lines)
                notmatched.append(line.strip())
        except StopIteration:
            return {
                "accepted": False,
                "description": "No results found",
                "messages": []
            }
        except AssertionError:
            return {
                "accepted": False,
                "description": "Could not parse results",
                "messages": [{
                    "format": "plain",
                    "description": "The output of our tests were badly formated, please contact the assistent."
                }]
            }


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
