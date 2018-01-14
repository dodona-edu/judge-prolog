import fileinput
import re
import json
from prologGeneral import checkErrors, swipl


selfCheckInfo = {
    "nl": """Jou feiten vergeleken met onze feiten op {numtests} gevallen, hievan waren er **{failed}** niet iedentiek. 

Hieronder zie je de de verwachte en uitgekomen output. Het inferentielimiet was {inferencelimit} en choisepoints waren {cpallowd} toegelaten. 
""",
    "en": """**Quckcheck** checked  **{numtests} predicates** over your code, **{failed}** of which failed. 

The results are below the inference limit was {inferencelimit} and choisepoint were {cpallowd}.
"""
}

class SimpleTest(object):
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

        # Make a new testfile that consults the users solution
        # and the check file
        self.testfileName = filename + ".extended.pl"
        

        with open(self.testfileName, "w") as f2:
            f2.write("""
        :- use_module("{judgePath}/simpletest/checker.pl").
        :- consult("{sumbissionPath}").
        :- load_files("{testfile}",[module(dodonaevaluate)]).
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
        failedTest = 0

        def oh(stdout, stderr, testname, scriptfile, config, timeout):
            testcases = []
            if timeout:
                testcases.append({
                    "accepted": False,
                    "description": "Timeout " + testname,
                    "messages": [{"format": "code", "description": "The test timed out (more than 1s)!\n\nstdOut:\n" + ("".join(stdout))}]
                })

            testcases += checkErrors(stderr, testname)
            return testcases

        outputContext = self._mkOutputContext(swipl(
            scriptfile=self.testfileName,
            testname="ll",
            goal="dodonacheck:dotests()",
            outputHandler=oh,
            timeout=5,
            config=self.config))
        


        results = []
        cpallowd = False
        inferencelimit = None
        with open(self.config["workdir"]+"/result.json",'r') as f:
            res = json.load(f)
            results = res["result"]
            cpallowd = res["allowcp"] == "true"
            inferencelimit = res["inferencelimit"]
            resultContext = self._mkResultContext(res)

        return {
            "badgeCount": failedTest,
            "description": self.tabname,
            "messages": [{
                "format": "markdown",
                "description": selfCheckInfo[self.lang].format(
                    numtests=12,
                    failed=failedTest,
                    inferencelimit=1000,
                    cpallowd=True
                )
            }],
            "groups": [resultContext,outputContext]
        }

    def _mkResultContext(self,res):

        tests = [
            {
                "generated":str(t["got"]),
                "expected":str(t["expected"]),
                "accepted":str(t["got"]) == str(t["expected"])
            }
            for t in res["result"]
        ]

        return {
            "accepted": False,
            "description": "Test results",
            "groups":[{
                "accepted": False,
                "description": " test",
                "tests": tests
            }]
        }


    def _mkOutputContext(self, testcases):
        return {
            "accepted": len(testcases) == 0,
            "description": "Std error",
            "groups": testcases,
            "messages": [{
                "format": "code",
                "description": "Error stuff"
            }]
        }



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
