"""Unit tests for the functions that turn the output of a swipl run into
feedback.

They get the output of real swipl 9.2.9 runs, kept in
tests/fixtures/<sub-judge>/<case>.{stdout,stderr,result.json} by
devel/capture-fixtures.sh, and don't need swipl.
"""

import os
import unittest

from helpers import TESTS_DIR, error_case
from formcheck import formcheckAnnotations, formcheckTestcases
from plunit import plunitTestcases
from prologGeneral import removeMountDir
from quickcheck import quickcheckResultTestcase, quickcheckTestcases
from simpltest import LANG as SIMPLETEST_LANG, simpletestTestcases
from util import readJson

FIXTURES_DIR = os.path.join(TESTS_DIR, "fixtures")
# The sandbox devel/capture-fixtures.sh runs the judge in.
SOURCE = "/mnt/sandbox/submission/source"


def fixture_path(name):
    return os.path.join(FIXTURES_DIR, name)


def read_fixture(name):
    """The fixture's text, or "" when the run wrote nothing there."""
    if not os.path.exists(fixture_path(name)):
        return ""
    with open(fixture_path(name)) as f:
        return f.read()


def swipl_output(case, removeMounts=True):
    """The stdout and stderr prologGeneral.swipl gives the output handler for
    a captured run."""
    stdout = read_fixture(case + ".stdout")
    stderr = read_fixture(case + ".stderr")
    if removeMounts:
        stdout, stderr = removeMountDir(stdout), removeMountDir(stderr)
    return {"stdout": stdout.splitlines(True), "stderr": stderr.splitlines(True)}


def output_case(description, text):
    return {"accepted": False, "description": description, "messages": [{"format": "code", "description": text}]}


class PLUnitTestcasesTest(unittest.TestCase):
    def feedback(self, case, testname):
        return plunitTestcases(testname=testname, timedOut=False, **swipl_output("plunit/" + case))

    def test_passing_tests_give_no_testcases(self):
        self.assertEqual(self.feedback("correct_realhug", "realhug"), [])

    def test_failed_assertion_without_the_test_file_path(self):
        self.assertEqual(self.feedback("assertion_failure_brohug", "brohug"), [
            error_case("ERROR", "test brohug:friends: assertion"),
            error_case("ERROR", "test brohug:friends: assertion at line 8 failed\n"
                                "Assertion: brohug(jan,tom)\n"),
            error_case("ERROR", "1 test failed"),
        ])

    def test_syntax_error_in_the_submission(self):
        self.assertEqual(self.feedback("syntax_error_realhug", "realhug"), [
            error_case("ERROR", "submission/source:1:27: Syntax error: Operator expected"),
            error_case("ERROR", "test realhug:mutual: assertion"),
            error_case("ERROR", "test realhug:mutual: assertion at line 9 raised "
                                '"catch/3: Unknown procedure: realhug/2"\n'
                                "Assertion: realhug(jan,fien)\n"),
            error_case("ERROR", "test realhug:one_sided: received error: "
                                "time:run_alarm_goal/2: Unknown procedure: realhug/2"),
            error_case("ERROR", "2 tests failed"),
        ])

    def test_stdout_comes_before_stderr(self):
        self.assertEqual(
            plunitTestcases(["ERROR: on stdout\n"], ["ERROR: on stderr\n"], "t", False),
            [error_case("ERROR", "on stdout"), error_case("ERROR", "on stderr")])

    def test_timeout_comes_first_with_the_output_so_far(self):
        self.assertEqual(plunitTestcases([], ["% [1/2] t:a ... passed\n", "ERROR: boom\n"], "t", True), [
            {
                "accepted": False,
                "description": "Timeout t",
                "messages": [{
                    "format": "code",
                    "description": "The test timed out (5s)!\n\nstdErr:\n% [1/2] t:a ... passed\nERROR: boom\n",
                }],
            },
            error_case("ERROR", "boom"),
        ])


class QuickCheckTestcasesTest(unittest.TestCase):
    TESTNAME = "prop_reverse_twice/1"
    COUNTEREXAMPLE_TABLE = (
        "\n"
        '<table class="table">\n'
        "    <caption>Counter example arguments</caption>\n"
        "    <thead> <tr> <th>#</th> <th>Type</th> <th>Value</th></tr> </thead>\n"
        "    <tbody><tr><td>0</td><td class='code'>list(integer)</td><td class='code'>[0]</td></tr></tbody>\n"
        "</table>")
    ARITHMETIC_ERROR = (
        "ERROR: =:=/2: Arithmetic: `two/0' is not a function\n"
        "\n"
        "   Exception: (4) quickcheck:run_tests(100, user, prop_double, "
        "[_17324:oneof([21])], _17380) ? EOF: exit (status 4)\n")
    PARSED_ARITHMETIC_ERROR = error_case(
        "ERROR",
        "=:=/2: Arithmetic: `two/0' is not a function\n"
        "   Exception: (4) quickcheck:run_tests(100, user, prop_double, "
        "[_17324:oneof([21])], _17380) ? EOF: exit (status 4)")
    NO_RESULT = {"accepted": False, "description": "Could not execute test"}

    def result(self, case):
        return readJson(fixture_path("quickcheck/{}.result.json".format(case)))

    def test_no_counterexample(self):
        self.assertEqual(
            quickcheckResultTestcase(self.result("correct_prop_reverse_twice"), "en"),
            {"accepted": True, "description": "All 100 tests passed, no counterexample found "})

    def test_no_counterexample_in_dutch(self):
        self.assertEqual(
            quickcheckResultTestcase(self.result("correct_prop_reverse_twice"), "nl"),
            {"accepted": True, "description": "Geen tegenvoorbeelden gevonden (100 testen geslaagd)"})

    def test_counterexample(self):
        self.assertEqual(quickcheckResultTestcase(self.result("wrong_prop_reverse_twice"), "en"), {
            "accepted": False,
            "description": {"format": "prolog", "description": "prop_reverse_twice([0])."},
            "tests": [{"generated": "false.", "expected": "true.", "accepted": False}],
            "messages": [{"description": self.COUNTEREXAMPLE_TABLE, "format": "html"}],
        })

    def test_counterexample_in_dutch(self):
        testcase = quickcheckResultTestcase(self.result("wrong_prop_reverse_twice"), "nl")
        self.assertIn("<caption>Waarden argumenten tegenvoorbeeld</caption>", testcase["messages"][0]["description"])

    def test_counterexample_values_are_escaped(self):
        testcase = quickcheckResultTestcase({
            "accepted": "false",
            "counterterm": "prop(<b>)",
            "counterparams": [{"type": "atom", "value": "<b>&"}],
        }, "en")
        self.assertEqual(testcase["description"]["description"], "prop(<b>).")
        self.assertIn("<td class='code'>atom</td><td class='code'>&lt;b&gt;&amp;</td>",
                      testcase["messages"][0]["description"])

    def test_result_comes_before_the_parsed_stderr(self):
        res = self.result("correct_prop_reverse_twice")
        self.assertEqual(
            quickcheckTestcases([], ["Warning: late\n"], self.TESTNAME, False, 10, res, "en"),
            [quickcheckResultTestcase(res, "en"), error_case("Warning", "late")])

    def test_without_result_the_output_is_shown(self):
        # The lines keep their newline and are joined with another one.
        self.assertEqual(
            quickcheckTestcases(["hello\n", "world\n"], [], self.TESTNAME, False, 10, None, "en"),
            [output_case("Could not execute test", "hello\n\nworld\n")])

    def test_without_result_the_error_is_parsed(self):
        self.assertEqual(
            quickcheckTestcases(timedOut=False, timeout=10, res=None, lang="en", testname="prop_double/1",
                                **swipl_output("quickcheck/arithmetic_error")),
            [self.NO_RESULT, self.PARSED_ARITHMETIC_ERROR])

    def test_without_result_or_output(self):
        self.assertEqual(
            quickcheckTestcases([], [], self.TESTNAME, False, 10, None, "en"),
            [self.NO_RESULT])

    def test_without_result_in_dutch(self):
        testcase, = quickcheckTestcases(["x\n"], [], self.TESTNAME, False, 10, None, "nl")
        self.assertEqual(testcase["description"], "Test kon niet worden uitgevoerd")

    def test_timeout_ignores_the_result(self):
        self.assertEqual(
            quickcheckTestcases(timedOut=True, timeout=10, res=self.result("correct_prop_reverse_twice"),
                                lang="en", testname="prop_double/1", **swipl_output("quickcheck/arithmetic_error")),
            [{
                "accepted": False,
                "description": "Timeout prop_double/1",
                "messages": [
                    {"format": "plain", "description": "The execution of this test exceeded the time limit (10s)!"},
                    {"format": "plain", "description": "StdErr:\n" + self.ARITHMETIC_ERROR},
                ],
            }, self.PARSED_ARITHMETIC_ERROR])

    def test_timeout_shows_and_parses_stderr(self):
        self.assertEqual(quickcheckTestcases([], ["Warning: late\n"], "prop/1", True, 7, None, "nl"), [
            {
                "accepted": False,
                "description": "Timeout prop/1",
                "messages": [
                    {"format": "plain", "description": "De tijdslimiet (7s) voor deze test werd overschreden!"},
                    {"format": "plain", "description": "StdErr:\nWarning: late\n"},
                ],
            },
            error_case("Warning", "late"),
        ])

    def test_syntax_error_is_parsed(self):
        self.assertIn(
            error_case("ERROR", "submission/source:1:23: Syntax error: Operator expected"),
            quickcheckTestcases(timedOut=False, timeout=10, res=None, lang="en", testname="prop_double/1",
                                **swipl_output("quickcheck/syntax_error")))


class SimpleTestTestcasesTest(unittest.TestCase):
    def feedback(self, case):
        return simpletestTestcases(testname="ll", timedOut=False, timeout=10, words=SIMPLETEST_LANG["en"],
                                   **swipl_output("simpltest/" + case))

    def test_no_output_gives_no_testcases(self):
        self.assertEqual(simpletestTestcases([], [], "ll", False, 10, SIMPLETEST_LANG["en"]), [])

    def test_output_of_the_submission(self):
        self.assertEqual(self.feedback("echo_output"), [
            output_case("Stdout ", "0123456789"),
        ])

    def test_syntax_error_is_parsed_and_shown_as_is(self):
        self.assertEqual(self.feedback("consult_syntax_error"), [
            error_case("ERROR", "submission/source:1:12: Syntax error: Operator expected"),
            output_case("Stderr ", "ERROR: submission/source:1:12: Syntax error: Operator expected\n"),
        ])

    def test_syntax_error_is_parsed(self):
        self.assertIn(
            error_case("ERROR", "submission/source:1:12: Syntax error: Operator expected"),
            self.feedback("consult_syntax_error"))

    def test_timeout_comes_first(self):
        self.assertEqual(simpletestTestcases(["out\n"], ["Warning: w\n"], "ll", True, 4, SIMPLETEST_LANG["nl"]), [
            {
                "accepted": False,
                "description": "Timeout",
                "messages": [{"format": "markdown", "description": "De test overschreed het tijdslimiet (4s)"}],
            },
            error_case("Warning", "w"),
            output_case("Stderr ", "Warning: w\n"),
            output_case("Stdout ", "out\n"),
        ])


class FormCheckTestcasesTest(unittest.TestCase):
    def test_no_timeout_gives_no_testcases(self):
        stderr = swipl_output("formcheck/singleton", removeMounts=False)["stderr"]
        self.assertEqual(formcheckTestcases(stderr, "check:check", False), [])

    def test_timeout_shows_the_output(self):
        stderr = swipl_output("formcheck/singleton", removeMounts=False)["stderr"]
        self.assertEqual(formcheckTestcases(stderr, "check:check", True), [{
            "accepted": False,
            "description": "Timeout check:check",
            "messages": [{
                "format": "code",
                "description": "The test timed out (more than 2s)!\n\nstdErr:\n"
                               "Warning: /mnt/sandbox/submission/source:1:\n"
                               "Warning:    Singleton variables: [Unused]\n",
            }],
        }])


class FormCheckAnnotationsTest(unittest.TestCase):
    def annotations(self, case=None, jsonRes=None):
        stderr = swipl_output("formcheck/" + case, removeMounts=False)["stderr"] if case else []
        return formcheckAnnotations(stderr, jsonRes, SOURCE)

    def test_nothing_found(self):
        self.assertEqual(self.annotations(), [])

    def test_singleton_warning(self):
        self.assertEqual(self.annotations("singleton"), [
            {"row": 0, "text": "Warning:    Singleton variables: [Unused]", "type": "warning"}])

    def test_warning_keeps_all_its_lines_and_names_the_submission(self):
        self.assertEqual(self.annotations("discontiguous"), [{
            "row": 4,
            "text": "Warning:    Clauses of grandparent/2 are not together in the source-file\n"
                    "Warning:    Earlier definition at submission.pl:1\n"
                    "Warning:    Current predicate: ancestor/2\n"
                    "Warning:    Use :- discontiguous grandparent/2. to suppress this message",
            "type": "warning",
        }])

    def test_each_output_message_is_an_annotation(self):
        stderr = [
            "Warning: {}:5:\n".format(SOURCE),
            "Warning:    Earlier definition at {}:1\n".format(SOURCE),
            "ERROR: {}:3:7: Syntax error: Operator expected\n".format(SOURCE),
        ]
        self.assertEqual(formcheckAnnotations(stderr, None, SOURCE), [
            {"row": 4, "text": "Warning:    Earlier definition at submission.pl:1", "type": "warning"},
            {"row": 2, "text": "Syntax error: Operator expected", "type": "error"},
        ])

    def test_output_about_other_files_is_ignored(self):
        stderr = ["Warning: /mnt/sandbox/resources/x.pl:3:\n", "Warning:    other\n"]
        self.assertEqual(formcheckAnnotations(stderr, None, SOURCE), [])

    def test_undefined_predicate_is_an_error(self):
        jsonRes = readJson(fixture_path("formcheck/undefined.result.json"))
        self.assertEqual(self.annotations(jsonRes=jsonRes), [{
            "row": 0,
            "text": "Undefined: ancestor/2, which is referenced by\n\t 1-st clause of grandparent/2",
            "type": "error",
        }])

    def test_linter_error_is_an_error(self):
        jsonRes = [{"msg": SOURCE + ":2:5: Syntax error: Operator expected", "type": "error"}]
        self.assertEqual(self.annotations(jsonRes=jsonRes), [
            {"row": 1, "text": "Error: Syntax error: Operator expected", "type": "error"}])

    def test_other_linter_messages_are_info(self):
        jsonRes = [{"msg": SOURCE + ":3: Clauses of p/1 are not together", "type": "discontiguous_clauses"}]
        self.assertEqual(self.annotations(jsonRes=jsonRes), [
            {"row": 2, "text": "Discontiguous Clauses: Clauses of p/1 are not together", "type": "info"}])

    def test_linter_message_loses_the_sandbox_prefix(self):
        jsonRes = [{"msg": SOURCE + ":2: Also defined in /mnt/sandbox/resources/x.pl", "type": "redefined"}]
        self.assertEqual(self.annotations(jsonRes=jsonRes), [
            {"row": 1, "text": "Redefined: Also defined in resources/x.pl", "type": "info"}])

    def test_linter_message_gets_an_annotation_per_location(self):
        jsonRes = [{"msg": "p/1, which is referenced by\n"
                           "\t" + SOURCE + ":1:5: 1-st clause of q/0\n"
                           "\t" + SOURCE + ":4:5: 1-st clause of r/0",
                    "type": "undefined"}]
        text = "Undefined: p/1, which is referenced by\n\t 1-st clause of q/0\n\t 1-st clause of r/0"
        self.assertEqual(self.annotations(jsonRes=jsonRes), [
            {"row": 0, "text": text, "type": "error"},
            {"row": 3, "text": text, "type": "error"},
        ])

    def test_output_messages_come_first(self):
        jsonRes = [{"msg": SOURCE + ":2: Clauses of p/1 are not together", "type": "discontiguous"}]
        self.assertEqual(self.annotations("singleton", jsonRes), [
            {"row": 0, "text": "Warning:    Singleton variables: [Unused]", "type": "warning"},
            {"row": 1, "text": "Discontiguous: Clauses of p/1 are not together", "type": "info"},
        ])


class ReadJsonTest(unittest.TestCase):
    def test_reads_the_file(self):
        self.assertEqual(readJson(fixture_path("quickcheck/correct_prop_reverse_twice.result.json")), {
            "accepted": "true",
            "counterparams": [],
            "counterterm": "null",
            "name": "prop_reverse_twice/1",
            "testcount": 100,
        })

    def test_missing_file(self):
        self.assertIsNone(readJson(fixture_path("quickcheck/missing.result.json")))

    def test_file_that_is_not_json(self):
        self.assertIsNone(readJson(fixture_path("quickcheck/syntax_error.stderr")))


if __name__ == "__main__":
    unittest.main()
