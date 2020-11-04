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
import os
import random
import re
from typing import Dict, Optional

from prologGeneral import swipl, removeMountDir
from util import removeFile

# This exact message confuses students, we will remove it from messages
BAD_UNDEFINED_MESSAGE = """The predicates below are not defined. If these are defined
at runtime using assert/1, use :- dynamic Name/Arity.

"""

LANG = {
    "en": {
        "tabname": "Format",
        "description": ("Your submission could not be accepted because it has an incorrect form. \n\n"
                        "You should resolve the issues below before we can accept your submission."),
        "tests": {
            "checkCut": {
                "title": "Usage of cut",
                "description": ("The usage of cut (`!`) is not required for this exercise. "
                                "Unneeded use of cut is considered to be bad style."),
                "treetext": "Save the tree, don't use cut <code>!</code>"
            },
            "checkCheat": {
                "title": "Cheater",
                "description": "Submissions may not use the module `dodonaevaluate`."
            },
            "checkChecker": {
                "title": "Problems found with `check:check/0`"
            }
        }
    },
    "nl": {
        "tabname": "Vorm",
        "description": ("Je oplossing kon niet aanvaard worden, omdat ze niet van de juiste vorm was.\n\n"
                        "Los de problemen hieronder op voordat je opnieuw indient."),
        "tests": {
            "checkCut": {
                "title": "Gebruik van cut",
                "description": ("Het gebruik van de cut (`!`) is niet nodig voor deze oefening. "
                                "Het is slechte stijl het te gebruiken als dat niet nodig is."),
                "treetext": "Red de boom, gebruik geen cut <code>!</code>"
            },
            "checkCheat": {
                "title": "Valsspeler",
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
        self.result = False  # type: Union[False,Optional[Dict]]
        self.annotations = []

        # Read input
        self.data = [x for x in fileinput.input(config["source"])]
        fileinput.close()

        # Add required tests
        self.tests = [self.checkCheat]
        if "predefined" in config:
            self.tests.append(self.checkChecker)
        if "cutallowed" in config and not config["cutallowed"]:
            self.tests.append(self.checkCut)

    def getResult(self) -> Dict:
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
            if random.random() > 0.9:
                # Fallen tree easter egg
                with open(os.sep.join([self.config["judge"], "fallen_tree.svg"])) as f:
                    msg = {
                        "format": "html",
                        "description": "<H2>" + texts["treetext"] + "</H2><br/>" + ("".join(f.readlines()))
                    }
            else:
                msg = {
                    "format": "markdown",
                    "description": texts["description"]
                }
            return {
                "accepted": False,
                "description": {
                    "format": "markdown",
                    "description": "#### " + texts["title"],
                },
                "messages": [msg],
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
            cases = []
            lints = []
            if timeout:
                cases.append({
                    "accepted": False,
                    "description": "Timeout " + testname,
                    "messages": [{
                        "format": "code",
                        "description":
                            "The test timed out (more than 2s)!" +
                            "\n\nstdOut:\n" + ("".join(stdout))
                    }]
                })

            if stdout:
                plFile = re.compile(re.escape(self.config["source"]))
                plResult = re.compile(
                    r"^(ERROR|Warning):\s+" + re.escape(self.config["source"]) + ":([0-9]+)(:([0-9]+))?:(.*)")
                curRes = {}
                for line in stdout:
                    isResult = plResult.match(line)
                    line = line.strip()
                    if isResult:
                        if curRes:
                            curRes["message"] = re.sub(
                                plFile, "submission.pl", curRes["message"])
                            lints.append(curRes)
                        curRes = {
                            "location": isResult.group(2),
                            "message": isResult.group(5).strip(),
                            "type": isResult.group(1)
                        }
                    else:
                        if curRes:
                            curRes["message"] += "\n" + line
                if curRes:
                    curRes["message"] = re.sub(
                        plFile, "submission.pl", curRes["message"])
                    lints.append(curRes)

            return cases, lints

        jsonOutputFile = self.config["workdir"] + "/result.json"
        removeFile(jsonOutputFile)  # ensure that the file is removed
        testCases, lints = swipl(
            scriptfile=testfilename,
            testname="check:check",
            goal="'dodona lint'",
            outputHandler=oh,
            timeout=2,
            config=self.config,
            removeMounts=False)

        jsonRes = None
        try:
            with open(jsonOutputFile, 'r') as f:
                try:
                    jsonRes = json.load(f)
                except json.decoder.JSONDecodeError:
                    pass
        except IOError:
            pass
        removeFile(jsonOutputFile)

        plResult = re.compile(
            re.escape(self.config["source"]) + r":([0-9]+)(:([0-9]+))?:?", )

        if jsonRes:
            for p in jsonRes:
                m = re.sub(plResult, "", p["msg"].strip())
                m = removeMountDir(m).strip()
                m = p["type"].replace("_", " ").title() + ": " + m

                if p["type"] == "undefined":
                    m = m.replace(BAD_UNDEFINED_MESSAGE, "", 1)

                errType = "error" if p["type"] in [
                    "undefined", "error"] else "info"

                for x in plResult.finditer(p["msg"]):
                    lints.append({
                        "location": x.group(1),
                        "message": m,
                        "type": errType
                    })

        for l in lints:
            errType = l["type"].lower()
            if errType not in ["error", "warning", "info"]:
                errType = "warning"

            r = {
                "row": int(l["location"]) - 1,
                "text": l["message"].strip(),
                "type": errType,
            }

            self.annotations.append(r)

        if testCases:
            return {
                "accepted": False,
                "description": {
                    "format": "markdown",
                    "description": "#### " + self.words["tests"]["checkChecker"]["title"]},
                "groups": testCases,
            }
        return None
