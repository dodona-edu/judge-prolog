"""
## Form check

The form check checks the form of submissions. This is basicly a linter

### Test specification

It is not driven by a test file,
but by the exercise configuration. Following keys can be inclused in the
`evaluation` directive:

- `cutallowed`: A boolean value indicationg if cuts are allowed (default false)
- `predefined`: List of predefined predicates in `Name/Arrity` form, if this is
   set, `check:check` will be executed (may be the empty list)

The checkCheat test is always executed, it looks for obeccurences of
`dodonaevaluate` and marks them as errorous.

example config
```
...
 "evaluation":{
    "predefined" : ["word/8"],
    "cutallowed": false
 },
...
```
"""

import fileinput
import json
import random
import re

from prologGeneral import swipl, removeMountDir, removeFile

FALENTREE = """<svg xmlns="http://www.w3.org/2000/svg" width="201" height="100" version="1"><path d="M42 56l31 5" fill="none" stroke="#000" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/><path d="M189 1c-13 7-31 12-49 14-2 2-2 5-1 8-8-4-17 1-21 12-2-4-12-5-18 2-10-1-24 2-25 17l-3 6c-7 0-10 4-8 11-3 5-2 9 1 14-4 1-10 7-10 14h140c7-36 6-72-6-98z" fill="#0e5d2d" fill-rule="evenodd" stroke="#000" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/><path d="M46 91c2-5-4-10-10-14l-4-16c-5 3-11 3-16 1-3 5-2 10-4 16C2 84 0 90 2 94c5-2 9-8 16-7l13 7c3-1 4-6 2-9 6 1 9 4 13 6zM36 44l-5 16 49 10h1c5-4 13-4 16 3 1-6 8-12 16-8 3-5 11-7 17-6l6-6c1-3-1-6-5-8-12 1-22 6-30 12-28-8-45-9-65-13z" fill="#784421" fill-rule="evenodd" stroke="#000" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/><path d="M32 61c0 1-4 3-8 3s-8-2-8-3c0-2 5-3 8-6l8 6zm3-17c2 0 2 3 2 8-1 4-3 7-5 7-1 0-1-5-4-9l7-6z" fill="#ba805a" stroke="#000" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/><path d="M113 64c-7-3-14 1-16 7-6-3-9-5-14-4-6 2-9 7-8 13 2-5 3-7 9-11 6-2 10 2 14 8 1-7 5-15 16-10 3-6 9-7 15-7 5 1 11 3 11 8h3c-1-7-8-9-14-10s-12 1-16 6zM68 53c8 1 16 2 25 5-19-2-20-3-25-5zm-29 5c11 1 21 2 31 6-23-2-24-4-31-6z" fill-rule="evenodd"/><path d="M73 63c7 1 15 1 23 4-17-1-18-3-23-4zM17 81c0-5 0-10 2-15 1 11-1 12-2 15zm13 1c0-5 0-9-2-15-1 11 1 12 2 15zm106 4c-6 1-9 5-10 11 2-5 5-8 10-9 5 0 9 2 11 7 0-7-5-10-11-9zm-47-4c-6 1-9 6-9 12 2-5 5-8 10-9s9 2 11 7c0-8-5-10-12-10zm28-47c-6 1-9 5-10 11 2-5 5-8 10-9 5 0 9 2 11 7 0-7-5-10-11-9z" fill-rule="evenodd"/><path d="M100 43c-6 1-8 4-9 9 2-4 5-6 9-6 5-1 8 1 9 4 1-5-4-8-9-7z" fill-rule="evenodd"/></svg>"""

LANG = {
    "en": {
        "tabname": "Format",
        "description": "Your submission cound not be accepted because it has an incorect form. \n\nThe issues below should be resolved before your submission can be accepted.",
        "tests": {
            "checkCut": {
                "title": "Usage of cut",
                "description": "The usage of cut (`!`) is not required for this exercise. Unneeded use of cut is considered to be bad style",
                "treetext": "Save the tree, don't use cut <code>!</code>"
            },
            "checkCheat": {
                "title": "Cheaters",
                "description": "Submissions may not use the module `dodonaevaluate`"
            },
            "checkChecker": {
                "title": "Problems found with `check:check/0`"
            }
        }
    },
    "nl": {
        "tabname": "Vorm",
        "description": "Jou oplossing kon niet aanvaard worden omdat er vorm fouten waren.\n\nLos de problemen hieronder op.",
        "tests": {
            "checkCut": {
                "title": "Gebruik van cut",
                "description": "Het gebruik van de cut (`!`) is niet nodig voor deze oefening. Het is slechte stijl het te gebruiken als dat niet nodig is.",
                "treetext": "Red de boom, gebruik geen cut <code>!</code>"
            },
            "checkCheat": {
                "title": "Valspeler",
                "description": "Het gebruik van de module `dodonaevaluate` is niet toegestaan door studentencode"
            },
            "checkChecker": {
                "title": "Problemen gevonden met `check:check/0`"
            }
        }
    }
}


class FormCheck(object):
    def __init__(self, config):
        self.config = config
        self.words = LANG[config["natural_language"]]
        self.tabname = self.words["tabname"]
        self.result = False
        self.annotations = []

        # Read input
        self.data = [x for x in fileinput.input(config["source"])]
        fileinput.close()

        self.tests = [self.checkCheat]
        if "predefined" in config:
            self.tests.append(self.checkChecker)
        if "cutallowed" in config and not config["cutallowed"]:
            self.tests.append(self.checkCut)

    def getResult(self):
        if self.result is False:
            self._doTest()
        return self.result

    def getAnnotations(self):
        if self.result is False:
            self._doTest()
        return self.annotations

    def getSummary(self):
        res = self.getResult()
        if res["badgeCount"] == 0:
            return "correct"
        else:
            return "FormCheck: {} issues".format(res["badgeCount"])

    def _doTest(self):

        results = [x for x in [f() for f in self.tests] if x is not None]

        if results:
            self.result = {
                "accepted": False,
                "badgeCount": len(results),
                "description": self.tabname,
                "messages": [{
                    "format": "markdown",
                    "description": self.words["description"]
                }],
                "groups": results,
            }
        else:
            self.result = None

    def checkCut(self):
        texts = self.words["tests"]["checkCut"]
        if any(["!" in line.split("%")[0] for line in self.data]):
            doTree = random.random() > 0.9
            return {
                "accepted": False,
                "description": {
                    "format": "markdown",
                    "description": "#### " + texts["title"],
                },
                "messages": [
                    {
                        "format": "html" if doTree else "markdown",
                        "description": (
                            "<H2>" +
                            texts["treetext"] +
                            "</H2><br/>" +
                            FALENTREE) if doTree else texts["description"]}],
            }
        return None

    def checkCheat(self):
        if any(["dodonaevaluate" in line for line in self.data]):
            texts = self.words["tests"]["checkCheat"]
            return {
                "accepted": False,
                "description": {
                    "format": "markdown",
                    "description": "#### " + texts["title"]
                },
                "messages": [{
                    "format": "markdown",
                    "description": texts["description"]
                }],
            }
        return None

    def checkChecker(self):
        testfilename = "/tmp/tmp-formcheck.pl"
        with open(testfilename, "w") as f2:
            f2.write("\n".join([":- dynamic {}.".format(x)
                                for x in self.config["predefined"]]) + "\n")
            f2.write("""
                :- use_module(library(check)).
                :- include("{judgePath}/linter/linter.pl").
                :- consult("{sumbissionPath}").
            """.format(
                judgePath=self.config["judge"],
                sumbissionPath=self.config["source"]
            ))

        def oh(stdout, testname, timeout, **kwargs):
            testcases = []
            lints = []
            if timeout:
                testcases.append({
                    "accepted": False,
                    "description": "Timeout " + testname,
                    "messages": [{
                        "format": "code",
                        "description":
                            "The test timed out (more than 1s)!" +
                            "\n\nstdOut:\n" + ("".join(stdout))
                    }]
                })

            if stdout:
                plResult = re.compile(
                    r"^(ERROR|Warning):\s+submission/source:([0-9]+)(:([0-9]+))?:(.*)")
                curRes = {}
                for line in stdout:
                    isResult = plResult.match(line)
                    line = line.strip()
                    if isResult:
                        if curRes:
                            lints.append(curRes)
                        curRes = {
                            "location": (isResult.group(2), isResult.group(4)),
                            "message": isResult.group(5).strip(),
                            "type": isResult.group(1)
                        }
                    else:
                        if curRes:
                            curRes["message"] += "\n" + line
                if curRes:
                    lints.append(curRes)

            return testcases, lints
        jsonOutputFile = self.config["workdir"] + "/result.json"
        removeFile(jsonOutputFile)
        testcases, lints = swipl(
            scriptfile=testfilename,
            testname="check:check",
            goal="'dodona lint'",
            outputHandler=oh,
            timeout=1,
            config=self.config)

        try:
            with open(jsonOutputFile, 'r') as f:
                try:
                    res = json.load(f)
                    removeFile(self.config["workdir"] + "/result.json")
                    plResult = re.compile(r"/mnt/[^:]*:([0-9]+)(:([0-9]+))?")
                    for p in res:
                        m = removeMountDir(p["msg"]).strip()
                        m = p["type"].replace("_", " ").title() + ": " + m
                        errType = "error" if p["type"] in ["undefined", "error"] else "info"

                        for x in plResult.finditer(p["msg"]):
                            lints.append({
                                "location": (x.group(1), x.group(3)),
                                "message": m,
                                "type": errType
                            })
                except json.decoder.JSONDecodeError:
                    pass
        except IOError:
            pass

        for l in lints:
            errType = l["type"].lower()
            if errType not in ["error", "warning", "info"]:
                errType = "warning"

            r = {
                "row": int(l["location"][0]) - 1,
                "text": l["message"].strip(),
                "type": errType,
            }
            if l["location"][1] is not None:
                r["col"] = int(l["location"][1]) - 1
            self.annotations.append(r)

        if testcases:
            return {
                "accepted": False,
                "description": {
                    "format": "markdown",
                    "description": "#### " + self.words["tests"]["checkChecker"]["title"]},
                "groups": testcases,
            }
        return None
