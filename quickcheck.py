import fileinput
import re
import os
import json
from prologGeneral import checkErrors, swipl


reProperty = re.compile(r"(prop_[^(]*)\((.*)\)\s*:-")
reBody = re.compile(r"^\s")

reBraces = re.compile(r"\([^()]*\)")

quickCheckInfo = {
    "nl": """**Quckcheck** controleerde **{numtests} predikaten** die allemaal waar zouden moeten zijn, hievan waren er **{failed} onwaar**.

Hieronder zie je de code die de predicaten voorstelt en als ze faalden een tegenvoorbeeld.
""",
    "en": """**Quckcheck** checked  **{numtests} predicates** that should be true, **{failed}** of which failed. 

The results below show the code that represents the predicates. If they fail, a counterexample is given.
"""
}


errorArgumentsTable = {
    "nl" : """
<table class="table">
    <caption>Waarden argumenten tegenvoorbeeld</caption>
    <thead> <tr> <th>#</th> <th>Type</th> <th>Waarde</th></tr> </thead>
    <tbody>{body}</tbody>
</table>""",
    "en" : """
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
        self.timeout = 1
        self.lang = config["natural_language"]
        self.result = None

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
            numBad = sum([not t["accepted"]
                          for t in testcases if "accepted" in t])

            context = {
                "accepted": numBad == 0,
                "description": {"description": "### {}".format(testname[5:].split("/")[0].replace("_", " ").title()), "format": "markdown"},
                "groups": testcases,
                "messages": [{
                    "format": "code",
                    "description": " \n"+"".join(self.properties[testname])+"\n"
                }]
            }

            failedTest += numBad == 0

            contexts.append(context)
            totalNumBad += numBad

        self.result = {
            "accepted": failedTest == 0,
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
        def oh(stdout, stderr, testname, timeout, **_):
            testcases = []
            if timeout:
                testcases.append({
                    "accepted": False,
                    "description": "Timeout " + testname,
                    "messages": [{"format": "code", "description": "The test timed out (more than 1s)!\n\nstdOut:\n" + ("".join(stdout))}]
                })
            else:
                try:
                    with open(self.config["workdir"]+"/result.json", 'r') as f:
                        res = json.load(f)
                        testcases.append(self._handleResult(res))
                except (IOError, json.decoder.JSONDecodeError):
                    testcases.append({
                        "accepted": False,
                        "description": "No testresults found " + testname,
                    })
                os.remove(self.config["workdir"]+"/result.json")

            testcases += checkErrors(stderr, testname)
            return testcases

        testcases = swipl(
            scriptfile=self.testfileName,
            testname=testname,
            goal="quickcheck({})".format(testname),
            outputHandler=oh,
            timeout=self.timeout,
            config=self.config)
        return testcases

    def _handleResult(self,res):
        if res["accepted"] == "true":
            return {
                "accepted": True,
                "description": "All {testcount} tests  passed ".format(**res),
            }
        else:
            rowfmt = "<tr><td>{i}</td><td class='code'>{type}</td><td class='code'>{value}</td></tr>"
            body = "".join([rowfmt.format(i=i, **arg)
                            for i, arg in enumerate(res["counterparams"])])
            
            tbl = errorArgumentsTable[self.lang].format(body=body)

            return {
                "accepted": False,
                "description":  {
                    "format": "code",
                    "description": res["counterterm"]+"."
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
    for c in params:
        if c in ['(', '[']:
            depth += 1
        elif c in [')', ']']:
            depth -= 1
        elif c == ',' and depth == 0:
            commas += 1
    return commas + 1
