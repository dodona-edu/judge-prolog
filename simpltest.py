"""Simple test: see "Format of Simple Test testfiles" in README.md."""
import re
import random
from prologGeneral import checkErrors, swipl
from util import readJson


LANG = {
    "en": {
        "description": {
            "failed": "We checked **{numtests} facts**, **{failed}** of them were incorrect.",
            "passed": "We checked **{numtests} facts**, they were all correct."},
        "hiddenrow": {
            True: "Another *{num} succeeded* tests are not listed.",
            False: "Another *{num} failed* tests are not listed."
        },
        "testresults": '#### Test results for "*{name}*"',
        "stdnotempty": "#### Output was not empty",
        "syntaxerror": "#### Syntax errors",
        "timeout": "The test timed out ({seconds}s)"
    },
    "nl": {
        "description": {
            "failed": "We controleerden **{numtests} feiten**, hievan waren er **{failed} niet correct**.",
            "passed": "We controleerden **{numtests} feiten**, ze waren allemaal correct. "},
        "hiddenrow": {
            True: "*{num}* andere *geslaagde* testen worden niet getoond.",
            False: "*{num}* andere *gefaalde* testen worden niet getoond."
        },
        "testresults": '#### Testresultaten voor "*{name}*"',
        "stdnotempty": "#### Output was niet leeg",
        "syntaxerror": "#### Syntax fouten",
        "timeout": "De test overschreed het tijdslimiet ({seconds}s)"
    }
}

# consult line regexp
# %- consult public file.
# %- consult private file.
# %- consult public [file,file].
# %- consult private [file,file].
consultRe = re.compile(
    r"^%-\s+consult\s+(public|private)\s+(\[\s*(\"([^\"]|\\\")+\"\s*,\s*)*\"([^\"]|\\\")+\"\s*\]|(\"([^\"]|\\\")+\")+)\s*\.\s*$")

# Number of succesfull (True) and failed (False) results to show
NUM_SHOW = {True: 5, False: 20}


class SimpleTest(object):
    def __init__(self, config, filename, tabname="QuickCheck"):
        self.config = config
        self.tabname = tabname
        self.timeout = 10
        self.lang = config["natural_language"]
        self.result = None
        self.words = LANG[self.lang]

        consults = {"public": [], "private": []}
        # Read input
        with open(filename) as f:
            for l in f:
                isConsult = consultRe.match(l)
                if isConsult:
                    consults[isConsult.group(1)].append(isConsult.group(2))

        # Make a new testfile that consults the users solution
        # and the check file
        self.testfileName = filename + ".extended.pl"

        with open(self.testfileName, "w") as f2:

            for c in consults["public"]:
                f2.write(':- consult({}).\n'.format(c))
            for c in consults["private"]:
                f2.write(':- consult({}{}).\n'.format(config["resources"], c))

            f2.write("""
            :- use_module("{judgePath}/simpletest/checker.pl").
            :- style_check(-singleton).
            :- style_check(-discontiguous).
            :- consult("{sumbissionPath}").
            :- use_module("{testfile}").
            """.format(
                judgePath=config["judge"],
                sumbissionPath=config["source"],
                testfile=filename
            ))

    def getResult(self):
        if self.result is None:
            self.result = self._doTest()
        return self.result

    def getAnnotations(self):
        return None

    def _doTest(self):
        outputContext = self._mkOutputContext(swipl(
            scriptfile=self.testfileName,
            testname="ll",
            goal="dodonacheck:dotests()",
            outputHandler=lambda stdout, stderr, testname, timeout, **_:
                simpletestTestcases(stdout, stderr, testname, timeout, self.timeout, self.words),
            timeout=self.timeout,
            config=self.config))

        res = readJson(self.config["workdir"] + "/result.json")

        resultContexts, numtests, failedTest = self._mkResultContext(res)

        return {
            "accepted": all([c["accepted"] for c in resultContexts + outputContext]),
            "badgeCount": failedTest,
            "description": self.tabname,
            "messages": [{
                "format": "markdown",
                "description": self.words["description"]["failed" if failedTest else "passed"].format(
                    numtests=numtests,
                    failed=failedTest
                )
            }],
            "groups": resultContexts + outputContext
        }

    def translate(self, text, result):
        """
        Translate the output of the checker program to results that the
        repl would show
        """
        translations = {
            "exit": "true.",
            "fail": "false.",
            "true": "true; (choice point remaining)",
            "inference_limit_exceeded": "Exceeded inference limit of {}".format(result["inferencelimit"]),
        }

        if text in translations:
            return translations[text]
        else:
            return text.replace("dodonacheck:", "")

    def _mkResultContext(self, res):
        numBadTotal = 0
        numTests = 0
        contexts = []
        if res is not None:

            for curResult in res:
                if curResult["allowcp"] == "true":
                    def transformer(x):
                        return "exit" if x == "true" else x
                else:
                    def transformer(x):
                        return x

                numBad = 0
                tests = {True: [], False: []}

                for t in curResult["result"]:
                    got = transformer(t["got"])
                    expected = transformer(t["expected"])
                    accepted = str(got) == str(expected)
                    if not accepted:
                        numBad += 1
                    tests[accepted].append(
                        {
                            "description": {
                                "format": "prolog",
                                "description": t["term"] + "."
                            },
                            "accepted": accepted,
                            "tests": [{
                                "generated": self.translate(got, curResult),
                                "expected": self.translate(expected, curResult),
                                "accepted": accepted
                            }]
                        }
                    )

                self.limitTests(tests)

                numBadTotal += numBad
                numTests += len(curResult["result"])
                contexts.append(
                    {
                        "accepted": numBad == 0,
                        "description": {
                            "format": "markdown",
                            "description": self.words["testresults"].format(name=curResult["name"])},
                        "groups": tests[False] + tests[True],
                    })
        else:
            contexts.append({
                "accepted": False,
                "description": "Test results",
                "messages": [{
                    "format": "markdown",
                    "description": "No results found"
                }, {
                    "format": "markdown",
                    "description": "This may be caused by a stack overflow",
                    "permission": "staff"
                }],
            })
        return contexts, numTests, numBadTotal

    def _mkOutputContext(self, testcases):
        if testcases:
            syntaxErrors = [t for t in testcases if ": Syntax error:" in t["messages"]
                            [0]["description"] and t["description"] == "ERROR"]
            if syntaxErrors:
                return [{
                    "accepted": False,
                    "description": {
                        "format": "markdown", "description": self.words["syntaxerror"]
                    },
                    "groups": syntaxErrors,
                }]
            else:
                return [{
                    "accepted": False,
                    "description": {
                        "format": "markdown", "description": self.words["stdnotempty"]
                    },
                    "groups": testcases,
                }]
        else:
            return []

    def limitTests(self, tests):
        for t in [True, False]:
            if len(tests[t]) > NUM_SHOW[t] + 1:
                tests[t] = random.sample(tests[t], NUM_SHOW[t]) + [{
                    "description": {
                        "format": "markdown",
                        "description": self.words["hiddenrow"][t].format(num=len(tests[t]) - NUM_SHOW[t])
                    },
                    "accepted": t,
                }]


def simpletestTestcases(stdout, stderr, testname, timedOut, timeout, words):
    testcases = []
    if timedOut:
        testcases.append({
            "accepted": False,
            "description": "Timeout",
            "messages": [{
                "format": "markdown",
                "description": words["timeout"].format(seconds=timeout)}]
        })

    testcases += checkErrors(stderr, testname)

    if stderr:
        testcases.append({
            "accepted": False,
            "description": "Stderr ",
            "messages": [{"format": "code", "description": "".join(stderr)}]
        })

    if stdout:
        testcases.append({
            "accepted": False,
            "description": "Stdout ",
            "messages": [{"format": "code", "description": "".join(stdout)}]
        })

    return testcases

