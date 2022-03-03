import fileinput
import html
import json
import re

from prologGeneral import checkErrors, swipl
from util import CondFormatString, removeFile

reProperty = re.compile(r"(prop_[^(]*)\((.*)\)\s*:-")
reBody = re.compile(r"^\s")

reBraces = re.compile(r"\([^()]*\)")

LANG = {
    "nl": {
        "info": CondFormatString(
            lambda **d: d["failed"] > 0,
            "**Quickcheck** controleerde **{numtests} predikaten** die altijd waar zouden moeten zijn. "
            " Er werden **{failed} tegenvoorbeelden** gevonden.\n\n"
            "Hieronder zie je de code die de predicaten voorstelt en de gevonden tegenvoorbeelden.",
            "**Quickcheck** controleerde **{numtests} predikaten** en vond geen tegenvoorbeelden."),
        "no_counter": "Geen tegenvoorbeelden gevonden ({testcount} testen geslaagd)",
        "no_results": "Test kon niet worden uitgevoerd",
        "timeout": "De tijdslimiet ({timeout}s) voor deze test werd overschreden!"
    },
    "en": {
        "info": CondFormatString(
            lambda **d: d["failed"] > 0,
            "**Quickcheck** validated  **{numtests} predicates** that should always be true. "
            "However, **{failed} counterexamples** were found.\n\n"
            "The results below show the code that represents the predicates. If they fail, a counterexample is shown.",
            "**Quickcheck** validated **{numtests} predicates** and could not find a counterexample."),
        "no_counter": "All {testcount} tests passed, no counterexample found ",
        "no_results": "Could not execute test",
        "timeout": "The execution of this test exceeded the time limit ({timeout}s)!"
    }
}

errorArgumentsTable = {
    "nl": """
<table class="table">
    <caption>Waarden argumenten tegenvoorbeeld</caption>
    <thead> <tr> <th>#</th> <th>Type</th> <th>Waarde</th></tr> </thead>
    <tbody>{body}</tbody>
</table>""",
    "en": """
<table class="table">
    <caption>Counter example arguments</caption>
    <thead> <tr> <th>#</th> <th>Type</th> <th>Value</th></tr> </thead>
    <tbody>{body}</tbody>
</table>"""

}


class QuickCheck(object):
    def __init__(self, config, filename, tabname="QuickCheck"):
        self.config = config
        self.tabname = tabname
        self.timeout = 10
        self.lang = config["natural_language"]
        self.result = None
        self.properties = {}
        self.orderedProperties = []

        # get property definitions
        self.getPropertyDefinitions(filename)

        # Make a new testfile that consults the users solution
        # and the check file
        self.testfileName = filename + ".extended.pl"
        consultLine = ':- consult("{}").\n'

        with open(self.testfileName, "w") as f2:
            f2.write(
                ':- style_check(-singleton).\n:- style_check(-discontiguous).\n')
            f2.write(consultLine.format(config["source"]))
            f2.write(consultLine.format(filename))
            f2.write(consultLine.format(
                self.config["judge"] + '/quicktest/quickcheck.pl'))

    def getPropertyDefinitions(self, filename):
        data = [l for l in fileinput.input(filename)]
        fileinput.close()
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

    def getResult(self):
        if self.result is None:
            self._doTest()
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
            numBad = sum([not t["accepted"] for t in testcases])

            context = {
                "accepted": numBad == 0,
                "description": {
                    "description": "#### {}".format(testname[5:].split("/")[0].replace("_", " ").title()),
                    "format": "markdown"
                },
                "groups": testcases,
                "messages": [{
                    "format": "prolog",
                    "description": " \n" + "".join(self.properties[testname]) + "\n"
                }]
            }

            failedTest += numBad > 0

            contexts.append(context)
            totalNumBad += numBad

        self.result = {
            "accepted": failedTest == 0,
            "badgeCount": failedTest,
            "description": self.tabname,
            "messages": [{
                "format": "markdown",
                "description": LANG[self.lang]["info"].format(
                    numtests=len(self.orderedProperties),
                    failed=failedTest
                )
            }],
            "groups": contexts
        }

    def _run(self, testname):
        outputJsonFile = self.config["workdir"] + "/result.json"

        def oh(stdout, stderr, testname, timeout, **_):
            testcases = []
            if timeout:
                messages = [LANG[self.lang]
                            ["timeout"].format(timeout=self.timeout)]
                if stdout:
                    messages.append("StdOut:\n" + "\n".join(stdout))
                if stderr:
                    messages.append("StdErr:\n" + "\n".join(stderr))

                timeoutCase = {
                    "accepted": False,
                    "description": "Timeout " + testname,
                    "messages": [{"format": "plain", "description": d} for d in messages]
                }

                testcases.append(timeoutCase)
            else:
                try:
                    with open(outputJsonFile, 'r') as f:
                        res = json.load(f)
                    removeFile(outputJsonFile)
                except (IOError, json.decoder.JSONDecodeError):
                    testcases.append({
                        "accepted": False,
                        "description": LANG[self.lang]["no_results"],
                        "messages": [{"format": "code", "description": "\n".join(stdout)}]
                    })
                else:
                    testcases.append(self._handleResult(res))
            testcases += checkErrors(stderr, testname)
            return testcases

        removeFile(outputJsonFile)
        testcases = swipl(
            scriptfile=self.testfileName,
            testname=testname,
            goal="quickcheck({})".format(testname),
            outputHandler=oh,
            timeout=self.timeout,
            config=self.config)
        return testcases

    def _handleResult(self, res):
        if res["accepted"] == "true":
            return {
                "accepted": True,
                "description": LANG[self.lang]["no_counter"].format(**res),
            }
        else:
            rowfmt = "<tr><td>{i}</td><td class='code'>{type}</td><td class='code'>{value}</td></tr>"
            body = "".join([
                rowfmt.format(i=i,
                              type=arg["type"],
                              value=html.escape(arg["value"]))
                for i, arg in enumerate(res["counterparams"])
            ])

            tbl = errorArgumentsTable[self.lang].format(body=body)

            return {
                "accepted": False,
                "description": {
                    "format": "prolog",
                    "description": res["counterterm"] + "."
                },
                "tests": [{
                    "generated": "false.",
                    "expected": "true.",
                    "accepted": False
                }],
                "messages": [
                    {
                        "description": tbl,
                        "format": "html"
                    }
                ]
            }


def countArgs(params):
    depth = 0
    commas = 0
    if params.replace(" ", "").strip() in ["", "()"]:
        return 0

    for c in params:
        if c in ['(', '[']:
            depth += 1
        elif c in [')', ']']:
            depth -= 1
        elif c == ',' and depth == 0:
            commas += 1
    return commas + 1
