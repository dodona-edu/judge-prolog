"""
## Simple test

Compares the output (that is true/false) of the students predicates with that
of a verified correct solution.

### Test specification

Test files ending in `.simple.pl` will automatically be be executed by this
file. Such a test file should be a module that exports one predicate: `test/6`.
The form is:

```prolog
%- consult public "database.pl".
:- module(dodonaevaluate, [tests/6]).

% ...

tests("markdown name",dodonaevaluate:thepredicate,thepredicate,true,10000, L) :-
    Pers = [waldo,odlaw,swi,wilma,whitebeard,woof,watchers],
    setof([X,Y],(member(X,Pers),member(Y,Pers)),L).
```

The `test/6` predicate has 6 arguments

1. The name, a string (double quotes), that is rendered as markdown
2. The functor name of a correct implementation (mostly of the form `dodonaevaluate:....`)
3. The functor name of the student implementation
4. A Boolean value indicating is cuts are allowed (does not work properly yet)
5. An inference limit
6. A list of argument lists
    - Example for a one argument predicate : [[1],[2],[3]]
    - Example for a two argument predicate : [[1,a],[2,a],[3,a]]
    - Example for a one argument predicate that takes lists : [[[1,a]],[[2,a,9,8]],[[3,a,4]]

To make files available to both the user and test code, use one of the following lines

```
%- consult public "file.pl".
%- consult private "file.pl".
%- consult public ["file1.pl","file2.pl"].
%- consult private ["file1.pl","file2.pl"].
```
### Implementation

The prolog checker code is contained in [simpletest/checker.pl](simpletest/checker.pl) it is loaded
together with the student code as follows, with a copy of the workspace folder as workspace and
`dotests` as goal for `swipl`.

```
:- use_module("{judgePath}/simpletest/checker.pl").
:- style_check(-singleton).
:- consult("/mnt/.../submission/submission.pl").
:- use_module("/mnt/.../evaluation/.....simple.pl").
```

The results stored in `workspace/results.json` are then consulted to build the feedback table
"""
import fileinput
import re
import json
import random
from prologGeneral import checkErrors, swipl, CondFormatString


LANG = {
    "en": {
        "description": CondFormatString(
            lambda **d: d["failed"] > 0,
            "We checked **{numtests} facts**, **{failed}** of them were incorrect.",
            "We checked **{numtests} facts**, they were all correct."),
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
        "description": CondFormatString(
            lambda **d: d["failed"] > 0,
            "We controleerden **{numtests} feiten**, hievan waren er **{failed} niet correct**.",
            "We controleerden **{numtests} feiten**, ze waren allemaal correct. "),
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
        self.bufsize = 2500
        self.numlines = 250
        self.lang = config["natural_language"]
        self.result = None
        self.words = LANG[self.lang]

        consults = {"public": [], "private": []}
        # Read input
        for l in fileinput.input(filename):
            l.strip()
            isConsult = consultRe.match(l)
            if isConsult:
                consults[isConsult.group(1)].append(isConsult.group(2))

        fileinput.close()

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

    def getSummary(self):
        res = self.getResult()
        if res["badgeCount"] == 0:
            return "correct"
        else:
            return "Simplecheck: {} issues".format(res["badgeCount"])

    def _doTest(self):
        def oh(stdout, stderr, testname, scriptfile, config, timeout):
            testcases = []
            if timeout:
                testcases.append({
                    "accepted": False,
                    "description": "Timeout",
                    "messages": [{
                        "format": "markdown",
                        "description": self.words["timeout"].format(seconds=self.timeout)}]
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

        outputContext = self._mkOutputContext(swipl(
            scriptfile=self.testfileName,
            testname="ll",
            goal="dodonacheck:dotests()",
            outputHandler=oh,
            timeout=self.timeout,
            config=self.config))

        res = None
        try:
            with open(self.config["workdir"] + "/result.json", 'r') as f:
                try:
                    res = json.load(f)
                except json.decoder.JSONDecodeError:
                    pass
        except IOError:
            pass

        resultContexts, numtests, failedTest = self._mkResultContext(res)

        return {
            "accepted": all([c["accepted"] for c in resultContexts + outputContext]),
            "badgeCount": failedTest,
            "description": self.tabname,
            "messages": [{
                "format": "markdown",
                "description": self.words["description"].format(
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
                    "accepted": len(testcases) == 0,
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


if __name__ == '__main__':
    tabs = [SimpleTest({
        "natural_language": "nl",
        "workdir": "/tmp",
        "source": "/home/beardhatcode/Documents/lp/judge/simpletest/example-submission.pl",
        "judge": "/home/beardhatcode/Documents/lp/judge",
        "prolog_local_stack": "128M",
        "prolog_global_stack": "128M",
        "prolog_trail_stack": "128M",
    }, "/home/beardhatcode/Documents/lp/judge/simpletest/example-evaluate.pl").getResult()]

    nb = sum([t["badgeCount"] for t in tabs])
    feedback = {
        "accepted": all([t["badgeCount"] == 0 for t in tabs]),
        "groups": tabs,
        "status": "correct answer" if nb == 0 else "wrong answer",
        "description": "issues({}).".format(nb) if nb > 0 else "true."
    }
    print(json.dumps(feedback, indent=2, separators=(',', ': ')))
