import fileinput
import re
import json
import random
from prologGeneral import checkErrors, swipl, removeMountDir


selfCheckInfo = {
    "nl": """We roepten controleerden **{numtests} feiten**, hievan hadden waren er **{failed} niet correct**. 

Hieronder zie je de de verwachte en uitgekomen output. 
""",
    "en": """We checked  **{numtests} facts**, **{failed}** of them were incorrect.

The results are below.
"""
}

hiddenRowInfo = {
    "nl" : {
        True : "*{num}* andere *geslaagde* testen worden niet getoond.",
        False: "*{num}* andere *gefaalde* testen worden niet getoond."
    },
    "en" : {
        True : "Another *{num} succeeded* tests are not listed.",
        False: "Another *{num} failed* tests are not listed."
    }
}


testResultsInfo = {
    "nl" : '### Testresultaten voor "*{name}*"',
    "en" : '### Test results for "*{name}*"'
}

stdErrInfo = {
    "nl" : "### Output was niet leeg",
    "en" : "### Output was not empty",
}

syntaxInfo = {
    "nl" : "### Syntax fouten",
    "en" : "### Syntax errors",
}

# consult line regexp
# %- consult public file.
# %- consult private file.
# %- consult public [file,file].
# %- consult private [file,file].
consultRe = re.compile(r"^%-\s+consult\s+(public|private)\s+(\[\s*(\"([^\"]|\\\")+\"\s*,\s*)*\"([^\"]|\\\")+\"\s*\]|(\"([^\"]|\\\")+\")+)\s*\.\s*$")

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

        consults = {"public": [], "private":[]}
        # Read input
        for l in fileinput.input(filename):
            l.strip()
            isConsult  = consultRe.match(l)
            if(isConsult):
                consults[isConsult.group(1)].append(isConsult.group(2))
            

        fileinput.close()

        # Make a new testfile that consults the users solution
        # and the check file
        self.testfileName = filename + ".extended.pl"
        
        with open(self.testfileName, "w") as f2:

            for c in consults["public"]:
                f2.write(':- consult({}).\n'.format(c))
            for c in consults["private"]:
                f2.write(':- consult({}{}).\n'.format(config["resources"],c))

            f2.write("""
        :- use_module("{judgePath}/simpletest/checker.pl").
        :- consult("{sumbissionPath}").
        :- use_module("{testfile}").
        """.format(
            judgePath = config["judge"],
            sumbissionPath = config["source"],
            testfile = filename
        ))

    def getResult(self):
        if self.result is None:
            self.result = self._doTest()
        return self.result

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
                    "description": "Timeout " + testname,
                    "messages": [{"format": "code", "description": "The test timed out (more than "+str(self.timeout)+"s)!\n\nstdOut:\n" + ("".join(stdout))}]
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
        


        results = []
        cpallowd = False
        inferencelimit = None
        res = None
        try:
            with open(self.config["workdir"]+"/result.json",'r') as f:
                try:
                    res = json.load(f)
                except json.decoder.JSONDecodeError:
                    pass
        except IOError:
            pass

        resultContexts,numtests, failedTest = self._mkResultContext(res)

        return {
            "accepted": all([c["accepted"] for c in outputContext+resultContexts]),
            "badgeCount": failedTest,
            "description": self.tabname,
            "messages": [{
                "format": "markdown",
                "description": selfCheckInfo[self.lang].format(
                    numtests=numtests,
                    failed=failedTest
                )
            }],
            "groups": outputContext+resultContexts
        }

    def translate(self,text,result):
        translations = {
            "exit" :  "true.",
            "fail" :  "false.",
            "true" :  "true; (checkpoints remaining)",
            "inference_limit_exceeded" :  "Exceeded inference limit of {}".format(result["inferencelimit"]),
        }

        if(text in translations):
            return translations[text]
        else:
            return text

    def _mkResultContext(self, res):
        numBadTotal = 0
        numTests = 0
        contexts = []
        if res is not None:

            for curResult in res:
                if curResult["allowcp"]:
                    transformer = lambda x: "exit" if x == "true" else x
                else:
                    transformer = lambda x: x
                numBad = 0
                tests = {True:[] , False:[]}
                
                for t in curResult["result"]:
                    got = transformer(t["got"])
                    expected = transformer(t["expected"])
                    accepted = str(got) == str(expected)
                    if not accepted:
                        numBad += 1
                    tests[accepted].append(
                        {
                            "description":  {
                                "format": "code", 
                                "description": t["term"]+"."
                                },
                            "accepted": accepted,
                            "tests": [{
                                "generated": self.translate(got,curResult),
                                "expected":self.translate(expected,curResult),
                                "accepted":accepted
                            }]
                        }
                    )

                self.limitTests(tests)

                numBadTotal += numBad
                numTests += len(curResult["result"])
                contexts.append({
                    "accepted": numBad == 0,
                    "description": {"format": "markdown", "description":testResultsInfo[self.lang].format(name = curResult["name"])},
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
            syntaxErrors = [t for t in testcases if ": Syntax error:" in t["messages"][0]["description"] and t["description"] == "ERROR"]
            if(syntaxErrors):
                return [{
                "accepted": False,
                "description": {
                    "format": "markdown", "description": syntaxInfo[self.lang]
                },
                "groups": syntaxErrors,
                }]
            else:
                return [{
                    "accepted": len(testcases) == 0,
                    "description": {
                        "format": "markdown", "description": stdErrInfo[self.lang]
                    },
                    "groups": testcases,
                }]
        else:
            return []



    def limitTests(self, tests):
        for t in [True, False]:
            if len(tests[t]) >  NUM_SHOW[t] + 1:
                tests[t] = random.sample(tests[t], NUM_SHOW[t]) + [{
                    "description":  {
                        "format": "markdown",
                        "description": hiddenRowInfo[self.lang][t].format(num=len(tests[t]) - NUM_SHOW[t])
                    },
                    "accepted": t,
                }]  
    

if __name__ == '__main__':
    t = SimpleTest({
        "natural_language" : "nl",
        "workdir" : "/tmp",
        "source" : "/home/beardhatcode/Documents/lp/judge/simpletest/example-submission.pl" ,
        "judge" : "/home/beardhatcode/Documents/lp/judge",
        "prolog_local_stack" : "128M",
        "prolog_global_stack" : "128M",
        "prolog_trail_stack" : "128M",
    },"/home/beardhatcode/Documents/lp/judge/simpletest/example-evaluate.pl")

    tests = [t]
    tabs = [t.getResult() for t in tests]
    numBad = sum([t["badgeCount"] for t in tabs])
    accepted = all([t["badgeCount"] == 0 for t in tabs])
    feedback = {
        "accepted": accepted, 
        "groups": tabs, 
        "status": "correct answer" if numBad == 0 else "wrong answer", 
        "description": "issues({}).".format(numBad) if numBad > 0 else "true."
        }
    print(json.dumps(feedback, indent=2, separators=(',', ': ')))
