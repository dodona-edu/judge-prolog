"""Unit tests for the functions that turn raw swipl output into feedback.

They call the functions directly and don't need swipl.
"""

import io
import os
import shutil
import tempfile
import threading
import unittest

from helpers import EXERCISES_DIR, error_case
from prologGeneral import checkErrors, removeMountDir, removePath
from quickcheck import QuickCheck, countArgs
from simpltest import NUM_SHOW, SimpleTest
from util import SilentLimitedBuffer


class RemoveMountDirTest(unittest.TestCase):
    def test_strips_every_sandbox_prefix(self):
        self.assertEqual(
            removeMountDir("/mnt/a1/submission/source:1 and /mnt/b2/resources/x.pl:2"),
            "submission/source:1 and resources/x.pl:2")

    def test_strips_only_the_first_directory_after_mnt(self):
        self.assertEqual(removeMountDir("/mnt/one/two/three"), "two/three")

    def test_leaves_other_paths(self):
        for text in ("/tmp/tmp-testfile.pl:3", "resources/x.pl", "mnt/abc/x", ""):
            self.assertEqual(removeMountDir(text), text)


class RemovePathTest(unittest.TestCase):
    def test_strips_test_file_with_line(self):
        self.assertEqual(removePath("ERROR: resources/x.unit.pl-slice.pl:12: boom", "t"), "ERROR:  boom")

    def test_strips_test_file_without_line(self):
        self.assertEqual(removePath("in resources/x.unit.pl-slice.pl here", "t"), "in  here")

    def test_keeps_files_that_only_look_like_the_test_file(self):
        self.assertEqual(removePath("resources/x.unit.pl-sliceXpl:7: x", "t"), "resources/x.unit.pl-sliceXpl:7: x")
        self.assertEqual(removePath("resources/x.pl:7: x", "t"), "resources/x.pl:7: x")

    def test_strips_mount_dir_first(self):
        self.assertEqual(removePath("/mnt/tmpx/submission/source:3: x", "t"), "submission/source:3: x")

    def test_strips_plunit_module_prefix(self):
        self.assertEqual(
            removePath("Unknown procedure: plunit_realhug:realhug/2", "realhug"),
            "Unknown procedure: realhug/2")

    def test_keeps_prefix_of_other_tests(self):
        self.assertEqual(
            removePath("plunit_other:foo/1", "realhug"),
            "plunit_other:foo/1")


class CheckErrorsTest(unittest.TestCase):
    def test_no_lines(self):
        self.assertEqual(checkErrors([], "t"), [])

    def test_progress_and_comment_lines_are_skipped(self):
        lines = [
            "% [1/2] realhug:mutual ........ passed (0.016 sec)\n",
            "% [2/2] realhug:one_sided ..... passed (0.000 sec)\n",
            "[1/1] without prefix ... passed\n",
            "%   anything starting with a percent sign\n",
            "....\n",
        ]
        self.assertEqual(checkErrors(lines, "realhug"), [])

    def test_error_with_continuation_lines_is_one_message(self):
        lines = [
            "ERROR: submission/source:3:\n",
            "ERROR:     first detail\n",
            "\tsecond detail\n",
            "\n",
            "ERROR: 1 test failed\n",
        ]
        self.assertEqual(checkErrors(lines, "t"), [
            error_case("ERROR", "submission/source:3:\nfirst detail\nsecond detail\n"),
            error_case("ERROR", "1 test failed"),
        ])

    def test_test_file_header_line_has_no_message_of_its_own(self):
        lines = [
            "ERROR: resources/a.unit.pl-slice.pl:8:\n",
            "ERROR:     test a:b: assertion\n",
        ]
        self.assertEqual(checkErrors(lines, "a"), [error_case("ERROR", "test a:b: assertion")])

    def test_lines_before_the_first_error_are_compilation_errors(self):
        self.assertEqual(
            checkErrors(["something odd\n", "% progress\n"], "t"),
            [error_case("compilation", "something odd")])

    def test_plain_line_after_error_is_part_of_the_message(self):
        lines = [
            "ERROR: Stack limit (0.2Gb) exceeded\n",
            "  Stack sizes: local: 0.2Gb\n",
            "    [3] user:p(a)\n",
        ]
        self.assertEqual(
            checkErrors(lines, "t"),
            [error_case("ERROR", "Stack limit (0.2Gb) exceeded\n  Stack sizes: local: 0.2Gb\n    [3] user:p(a)")])

    def test_warning_and_error_are_separate_messages(self):
        lines = [
            "Warning: submission/source:1: Singleton variables: [X]\n",
            "ERROR: submission/source:2: boom\n",
        ]
        self.assertEqual(checkErrors(lines, "t"), [
            error_case("Warning", "submission/source:1: Singleton variables: [X]"),
            error_case("ERROR", "submission/source:2: boom"),
        ])

    def test_warning_continuation_lines_are_one_message(self):
        lines = [
            "Warning: submission/source:5:\n",
            "Warning:    Redefined static procedure parent/2\n",
            "Warning:    Previously defined at submission/source:1\n",
        ]
        self.assertEqual(checkErrors(lines, "t"), [
            error_case("Warning", "submission/source:5:\n"
                       "Redefined static procedure parent/2\n"
                       "Previously defined at submission/source:1"),
        ])

    def test_truncation_notice_is_a_message_of_its_own(self):
        # util.SilentLimitedBuffer's notice has four spaces, one short of a continuation line.
        lines = [
            "ERROR: first\n",
            "\n",
            "ERROR:    TRUNCATED 150 bytes ignored",
        ]
        self.assertEqual(
            checkErrors(lines, "t"),
            [error_case("ERROR", "first\n"), error_case("ERROR", "TRUNCATED 150 bytes ignored")])

    def test_progress_line_ends_the_current_message(self):
        lines = [
            "ERROR: first\n",
            "% [2/2] t:b ... passed\n",
            "ERROR: second\n",
        ]
        self.assertEqual(
            checkErrors(lines, "t"),
            [error_case("ERROR", "first"), error_case("ERROR", "second")])

    def test_type_of_the_last_header_sticks_to_later_plain_lines(self):
        lines = ["Warning: w\n", "% progress\n", "orphan line\n"]
        self.assertEqual(
            checkErrors(lines, "t"),
            [error_case("Warning", "w"), error_case("Warning", "orphan line")])

    def test_trailing_whitespace_is_removed(self):
        self.assertEqual(
            checkErrors(["ERROR: boom   \r\n"], "t"),
            [error_case("ERROR", "boom")])


class CountArgsTest(unittest.TestCase):
    def test_no_arguments(self):
        for params in ("", "  ", "()", " ( ) "):
            self.assertEqual(countArgs(params), 0, repr(params))

    def test_flat_arguments(self):
        self.assertEqual(countArgs("X"), 1)
        self.assertEqual(countArgs("X, Y"), 2)
        self.assertEqual(countArgs("A:list(integer), B:list(integer), C"), 3)

    def test_commas_inside_brackets_do_not_count(self):
        self.assertEqual(countArgs("[a,b,c]"), 1)
        self.assertEqual(countArgs("f(a, b), [c, d], e"), 3)
        self.assertEqual(countArgs("X:oneof([1,2,3]), Y"), 2)


class GetPropertyDefinitionsTest(unittest.TestCase):
    def setUp(self):
        self.dir = tempfile.mkdtemp(prefix="qc-")
        self.addCleanup(shutil.rmtree, self.dir, ignore_errors=True)

    def parse(self, text):
        path = os.path.join(self.dir, "x.qc.pl")
        with open(path, "w") as f:
            f.write(text)
        config = {"natural_language": "en", "source": "/s", "judge": "/j", "workdir": self.dir}
        return QuickCheck(config, path)

    def test_properties_of_the_reverse_exercise(self):
        path = os.path.join(EXERCISES_DIR, "quickcheck-reverse", "evaluation", "reverse.qc.pl")
        with open(path) as f:
            qc = self.parse(f.read())
        self.assertEqual(
            list(qc.properties),
            ["prop_reverse_twice/1", "prop_reverse_length/1", "prop_reverse_append/2"])
        self.assertEqual(
            "".join(qc.properties["prop_reverse_length/1"]),
            "prop_reverse_length(L:list(integer)) :-\n"
            "    % A reversed list is as long as the original.\n"
            "    my_reverse(L, R),\n"
            "    length(L, N),\n"
            "    length(R, N).\n")

    def test_property_at_end_of_file_without_blank_line(self):
        qc = self.parse("prop_a(X:integer) :-\n    X >= 0.")
        self.assertEqual(list(qc.properties), ["prop_a/1"])
        self.assertEqual(qc.properties["prop_a/1"], ["prop_a(X:integer) :-\n", "    X >= 0."])

    def test_other_blocks_are_ignored(self):
        qc = self.parse(":- use_module(library(lists)).\nhelper(X) :- X > 1.\n\nprop_a(X:integer) :-\n    helper(X).\n")
        self.assertEqual(list(qc.properties), ["prop_a/1"])

    def test_same_name_and_arity_are_merged_in_order(self):
        qc = self.parse(
            "prop_a(X:integer) :-\n    X > 0.\n\n"
            "prop_b(X:integer, Y:integer) :-\n    X < Y.\n\n"
            "prop_a(X:integer) :-\n    X < 0.\n")
        self.assertEqual(list(qc.properties), ["prop_a/1", "prop_b/2"])
        self.assertEqual(
            qc.properties["prop_a/1"],
            ["prop_a(X:integer) :-\n", "    X > 0.\n", "prop_a(X:integer) :-\n", "    X < 0.\n"])

    def test_same_name_with_other_arity_is_another_property(self):
        qc = self.parse("prop_a(X:integer) :-\n    true.\n\nprop_a(X:integer, Y:integer) :-\n    true.\n")
        self.assertEqual(list(qc.properties), ["prop_a/1", "prop_a/2"])

    def test_arity_ignores_commas_in_compound_types(self):
        qc = self.parse("prop_a(L:list(oneof([a,b])), N:integer) :-\n    true.\n")
        self.assertEqual(list(qc.properties), ["prop_a/2"])

    def test_blank_lines_with_crlf(self):
        qc = self.parse("prop_a(X:integer) :-\r\n    true.\r\n\r\nprop_b(X:integer) :-\r\n    true.\r\n")
        self.assertEqual(list(qc.properties), ["prop_a/1", "prop_b/1"])


class SimpleTestHelpersTest(unittest.TestCase):
    def setUp(self):
        self.dir = tempfile.mkdtemp(prefix="simple-")
        self.addCleanup(shutil.rmtree, self.dir, ignore_errors=True)
        source = os.path.join(EXERCISES_DIR, "simple-consult", "evaluation", "test.simple.pl")
        self.testfile = os.path.join(self.dir, "test.simple.pl")
        shutil.copyfile(source, self.testfile)

    def simple_test(self, language="en"):
        return SimpleTest({
            "natural_language": language,
            "resources": self.dir + "/",
            "source": "/mnt/x/submission/source",
            "judge": "/mnt/x/judge",
            "workdir": self.dir,
        }, self.testfile)

    @staticmethod
    def row(term, expected, got):
        return {"term": term, "expected": expected, "got": got}

    def result(self, rows, allowcp="false", name="demo"):
        return [{"allowcp": allowcp, "inferencelimit": 10000, "name": name, "result": rows}]

    def test_translate_known_results(self):
        st = self.simple_test()
        result = {"inferencelimit": 5000}
        self.assertEqual(st.translate("exit", result), "true.")
        self.assertEqual(st.translate("fail", result), "false.")
        self.assertEqual(st.translate("true", result), "true; (choice point remaining)")
        self.assertEqual(st.translate("inference_limit_exceeded", result), "Exceeded inference limit of 5000")

    def test_translate_other_text_loses_the_module_prefix(self):
        st = self.simple_test()
        self.assertEqual(
            st.translate("existence_error(procedure,dodonacheck:stuff/2) in context(dodonacheck:x,_1)", {"inferencelimit": 1}),
            "existence_error(procedure,stuff/2) in context(x,_1)")

    def test_result_context_without_results(self):
        contexts, numtests, failed = self.simple_test()._mkResultContext(None)
        self.assertEqual((numtests, failed), (0, 0))
        self.assertEqual(len(contexts), 1)
        self.assertFalse(contexts[0]["accepted"])
        self.assertEqual(contexts[0]["messages"][0]["description"], "No results found")
        self.assertEqual(contexts[0]["messages"][1]["permission"], "educator")

    def test_result_context_counts_and_orders_rows(self):
        rows = [
            self.row("p(1)", "exit", "exit"),
            self.row("p(2)", "exit", "fail"),
            self.row("p(3)", "fail", "fail"),
        ]
        contexts, numtests, failed = self.simple_test()._mkResultContext(self.result(rows))
        self.assertEqual((numtests, failed), (3, 1))
        context = contexts[0]
        self.assertFalse(context["accepted"])
        self.assertEqual(context["description"]["description"], '#### Test results for "*demo*"')
        self.assertEqual(
            [(g["description"]["description"], g["accepted"]) for g in context["groups"]],
            [("p(2).", False), ("p(1).", True), ("p(3).", True)])
        self.assertEqual(context["groups"][0]["tests"], [
            {"generated": "false.", "expected": "true.", "accepted": False}])

    def test_choice_point_is_wrong_when_not_allowed(self):
        rows = [self.row("p(1)", "exit", "true")]
        contexts, _, failed = self.simple_test()._mkResultContext(self.result(rows, allowcp="false"))
        self.assertEqual(failed, 1)
        self.assertEqual(
            contexts[0]["groups"][0]["tests"],
            [{"generated": "true; (choice point remaining)", "expected": "true.", "accepted": False}])

    def test_choice_point_is_right_when_allowed(self):
        rows = [self.row("p(1)", "exit", "true")]
        contexts, _, failed = self.simple_test()._mkResultContext(self.result(rows, allowcp="true"))
        self.assertEqual(failed, 0)
        self.assertEqual(
            contexts[0]["groups"][0]["tests"],
            [{"generated": "true.", "expected": "true.", "accepted": True}])

    def test_result_context_per_test_group(self):
        results = self.result([self.row("p(1)", "exit", "exit")], name="one") \
            + self.result([self.row("q(1)", "fail", "exit")], name="two")
        contexts, numtests, failed = self.simple_test()._mkResultContext(results)
        self.assertEqual((numtests, failed), (2, 1))
        self.assertEqual([c["accepted"] for c in contexts], [True, False])

    def test_output_context_without_output(self):
        self.assertEqual(self.simple_test()._mkOutputContext([]), [])

    def test_output_context_with_output(self):
        cases = [error_case("Stdout ", "text")]
        context, = self.simple_test()._mkOutputContext(cases)
        self.assertFalse(context["accepted"])
        self.assertEqual(context["description"]["description"], "#### Output was not empty")
        self.assertEqual(context["groups"], cases)

    def test_output_context_shows_only_syntax_errors_when_present(self):
        syntax = error_case("ERROR", "submission/source:1:12: Syntax error: Operator expected")
        other = error_case("Warning", "something else")
        context, = self.simple_test("nl")._mkOutputContext([other, syntax])
        self.assertEqual(context["description"]["description"], "#### Syntax fouten")
        self.assertEqual(context["groups"], [syntax])

    def test_syntax_error_text_in_a_warning_is_not_a_syntax_error(self):
        warning = error_case("Warning", "x: Syntax error: y")
        context, = self.simple_test()._mkOutputContext([warning])
        self.assertEqual(context["description"]["description"], "#### Output was not empty")

    def sample_tests(self, passed, failed):
        return {
            True: [{"accepted": True, "id": i} for i in range(passed)],
            False: [{"accepted": False, "id": i} for i in range(failed)],
        }

    def test_limit_tests_keeps_short_lists(self):
        tests = self.sample_tests(NUM_SHOW[True] + 1, NUM_SHOW[False] + 1)
        self.simple_test().limitTests(tests)
        self.assertEqual(len(tests[True]), NUM_SHOW[True] + 1)
        self.assertEqual(len(tests[False]), NUM_SHOW[False] + 1)
        self.assertTrue(all("id" in t for t in tests[True] + tests[False]))

    def test_limit_tests_samples_long_lists_and_adds_a_notice(self):
        tests = self.sample_tests(NUM_SHOW[True] + 2, NUM_SHOW[False] + 7)
        self.simple_test().limitTests(tests)
        self.assertEqual(len(tests[True]), NUM_SHOW[True] + 1)
        self.assertEqual(len(tests[False]), NUM_SHOW[False] + 1)
        self.assertEqual(tests[True][-1], {
            "description": {"format": "markdown", "description": "Another *2 succeeded* tests are not listed."},
            "accepted": True})
        self.assertEqual(tests[False][-1], {
            "description": {"format": "markdown", "description": "Another *7 failed* tests are not listed."},
            "accepted": False})
        ids = [t["id"] for t in tests[False][:-1]]
        self.assertEqual(len(set(ids)), NUM_SHOW[False])

    def test_limit_tests_notice_is_translated(self):
        tests = self.sample_tests(NUM_SHOW[True] + 3, 0)
        self.simple_test("nl").limitTests(tests)
        self.assertEqual(
            tests[True][-1]["description"]["description"],
            "*3* andere *geslaagde* testen worden niet getoond.")


class SilentLimitedBufferTest(unittest.TestCase):
    class Stream(io.BytesIO):
        """Sets `drained` when the reader asks for more after the last chunk."""

        def __init__(self, data):
            super().__init__(data)
            self.drained = threading.Event()

        def read(self, size=-1):
            data = super().read(size)
            if not data:
                self.drained.set()
            return data

    def read_all(self, data, maxsize=5000):
        stream = self.Stream(data)
        buffer = SilentLimitedBuffer(maxsize=maxsize).procces(stream)
        self.assertTrue(stream.drained.wait(5), "reader did not finish")
        return buffer.retreive_and_stop()

    def test_short_text_is_unchanged(self):
        buffer = SilentLimitedBuffer(maxsize=10)
        buffer.write("12345")
        buffer.write("678")
        self.assertEqual(buffer.getvalue(), "12345678")

    def test_text_over_the_limit_is_cut_and_reports_the_size(self):
        buffer = SilentLimitedBuffer(maxsize=10)
        buffer.write("12345678")
        buffer.write("abcdef")
        buffer.write("x")
        self.assertEqual(
            buffer.getvalue(),
            "12345678\n\nERROR:    TRUNCATED 7 bytes ignored")

    def test_text_of_exactly_the_limit_is_kept(self):
        buffer = SilentLimitedBuffer(maxsize=5)
        buffer.write("12345")
        self.assertEqual(buffer.getvalue(), "12345")

    def test_reads_a_stream_in_chunks(self):
        self.assertEqual(self.read_all(b"line\n" * 100), "line\n" * 100)

    def test_long_stream_is_truncated(self):
        value = self.read_all(b"a" * 250, maxsize=100)
        self.assertEqual(value, "a" * 100 + "\n\nERROR:    TRUNCATED 150 bytes ignored")

    def test_multibyte_characters_inside_a_chunk(self):
        text = "é" * 60 + "a" + "€" * 20
        self.assertEqual(self.read_all(text.encode("utf-8")), text)

    # Bug #28: output loses a multi-byte character cut off by a 100-byte read
    def test_multibyte_character_split_over_two_reads(self):
        text = "a" * 99 + "é" + "b" * 5
        self.assertEqual(self.read_all(text.encode("utf-8")), text)

    def test_invalid_bytes_are_dropped(self):
        self.assertEqual(self.read_all(b"\xffabcd"), "abcd")

    def test_empty_stream(self):
        self.assertEqual(self.read_all(b""), "")


if __name__ == "__main__":
    unittest.main()
