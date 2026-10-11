"""Runs the judge on every submission in tests/exercises and compares its
feedback with the expected output.

Meant to run as root inside the judge image. Set UPDATE_EXPECTED=1 to
(re)write the expected output instead of comparing against it.
"""

import contextlib
import difflib
import fnmatch
import glob
import json
import os
import re
import shutil
import subprocess
import tempfile
import unittest

from helpers import EXERCISES_DIR, REPO_DIR

UPDATE_EXPECTED = os.environ.get("UPDATE_EXPECTED") == "1"
RANDOM_SEED = 1
QUICKCHECK_RUNS = 5
JUDGE_TIMEOUT = 120
LANGUAGES = ("en", "nl")

# Variable numbers and stack overflow reports change between SWI-Prolog versions.
SWIPL_DETAILS = [
    (re.compile(r"(?<!\w)_[0-9]+\b"), "_"),
    (re.compile(r"Stack sizes: .*"), "Stack sizes: <sizes>"),
    (re.compile(r"Stack depth: .*"), "Stack depth: <depth>"),
    (re.compile(r"\[[0-9,]+\] (?=\S+:)"), "[<depth>] "),
]


def judge_ignore():
    """A shutil.copytree ignore function that leaves out what Dodona's sync
    of the judge leaves out."""
    with open(os.path.join(REPO_DIR, ".dodonaignore")) as f:
        lines = [line.strip() for line in f]
    patterns = [line for line in lines if line and not line.startswith("#")]

    def ignore(directory, names):
        # Not tracked, so Dodona never syncs them.
        ignored = {".git", ".claude", "__pycache__"}
        for name in names:
            path = "/" + os.path.relpath(os.path.join(directory, name), REPO_DIR)
            if os.path.isdir(os.path.join(directory, name)):
                path += "/"
            # Only anchored patterns; a trailing slash matches directories only.
            if any(fnmatch.fnmatchcase(path, p) or fnmatch.fnmatchcase(path, p + "/")
                   for p in patterns):
                ignored.add(name)
        return ignored

    return ignore


def make_sandbox_root():
    # Dodona mounts everything under /mnt/<random>/ and the judge strips that
    # prefix from its messages.
    if not os.access("/mnt", os.W_OK):
        raise RuntimeError("/mnt is not writable: run the suite as root, like CI")
    return tempfile.mkdtemp(dir="/mnt")


@contextlib.contextmanager
def sandbox(root, exercise_dir, submission):
    """Lays out the resources, submission and workdir for one run, and removes
    them afterwards."""
    resources = os.path.join(root, "resources")
    source = os.path.join(root, "submission", "source")
    workdir = os.path.join(tempfile.gettempdir(), "workdir")
    try:
        shutil.copytree(os.path.join(exercise_dir, "evaluation"), resources)
        os.makedirs(os.path.dirname(source))
        shutil.copyfile(submission, source)
        exercise_workdir = os.path.join(exercise_dir, "workdir")
        if os.path.isdir(exercise_workdir):
            shutil.copytree(exercise_workdir, workdir)
        else:
            os.mkdir(workdir)
        yield {"resources": resources, "source": source, "workdir": workdir}
    finally:
        for path in (resources, os.path.dirname(source), workdir):
            shutil.rmtree(path, ignore_errors=True)


def normalise(value):
    if isinstance(value, str):
        for pattern, replacement in SWIPL_DETAILS:
            value = pattern.sub(replacement, value)
        return value
    if isinstance(value, list):
        return [normalise(v) for v in value]
    if isinstance(value, dict):
        return {k: normalise(v) for k, v in value.items()}
    return value


def dump(value):
    return json.dumps(value, indent=2, ensure_ascii=False) + "\n"


def diff(a, b, a_name, b_name):
    return "".join(difflib.unified_diff(
        a.splitlines(True), b.splitlines(True), a_name, b_name))


def cases():
    for exercise in sorted(os.listdir(EXERCISES_DIR)):
        exercise_dir = os.path.join(EXERCISES_DIR, exercise)
        for submission in sorted(glob.glob(os.path.join(exercise_dir, "submissions", "*.pl"))):
            yield exercise, os.path.basename(submission)[:-len(".pl")]


class ExerciseTest(unittest.TestCase):
    maxDiff = None

    @classmethod
    def setUpClass(cls):
        cls.root = make_sandbox_root()
        cls.addClassCleanup(shutil.rmtree, cls.root)
        cls.judge = os.path.join(cls.root, "judge")
        shutil.copytree(REPO_DIR, cls.judge, ignore=judge_ignore())

    def run_judge(self, exercise_dir, submission, language):
        config = {
            "time_limit": 10,
            "memory_limit": 536870912,
        }
        config_file = os.path.join(exercise_dir, "config.json")
        if os.path.exists(config_file):
            with open(config_file) as f:
                config.update(json.load(f))
        config.update({
            "programming_language": "prolog",
            "natural_language": language,
            "judge": self.judge,
            "random_seed": RANDOM_SEED,
        })
        with sandbox(self.root, exercise_dir, submission) as paths:
            config.update(paths)
            return subprocess.run(
                ["/main.sh", os.path.join(self.judge, "run")],
                input=json.dumps(config),
                capture_output=True,
                text=True,
                cwd=paths["workdir"],
                timeout=JUDGE_TIMEOUT)

    def feedback(self, exercise_dir, submission, language):
        """The normalised feedback of one judge run, as JSON text."""
        process = self.run_judge(exercise_dir, submission, language)
        self.assertEqual(
            process.returncode, 0,
            "judge exited with {}\nstderr:\n{}".format(process.returncode, process.stderr))
        self.assertEqual(process.stderr, "", "judge wrote to stderr")
        try:
            feedback = json.loads(process.stdout)
        except json.JSONDecodeError as e:
            self.fail("judge output is not JSON ({})\nstdout:\n{}".format(e, process.stdout))
        return dump(normalise(feedback))

    def compare(self, expected_file, output):
        if not os.path.exists(expected_file):
            self.fail("no expected output, run with UPDATE_EXPECTED=1 to create "
                      + os.path.relpath(expected_file, REPO_DIR))
        with open(expected_file) as f:
            expected = dump(json.load(f))
        if output != expected:
            self.fail("output differs from the expected output:\n"
                      + diff(expected, output, "expected", "actual"))

    def check_case(self, exercise, case):
        exercise_dir = os.path.join(EXERCISES_DIR, exercise)
        submission = os.path.join(exercise_dir, "submissions", case + ".pl")
        expected_file = os.path.join(exercise_dir, "expected", case + ".json")
        suffix = os.path.splitext(case)[1].lstrip(".")
        language = suffix if suffix in LANGUAGES else "en"
        # QuickCheck picks its test values inside swipl, which random_seed doesn't reach.
        runs = QUICKCHECK_RUNS if glob.glob(
            os.path.join(exercise_dir, "evaluation", "*.qc.pl")) else 1

        outputs = [self.feedback(exercise_dir, submission, language) for _ in range(runs)]
        for i, output in enumerate(outputs[1:], start=2):
            if output != outputs[0]:
                self.fail("run {} differs from run 1:\n{}".format(
                    i, diff(outputs[0], output, "run 1", "run {}".format(i))))

        if UPDATE_EXPECTED:
            os.makedirs(os.path.dirname(expected_file), exist_ok=True)
            with open(expected_file, "w") as f:
                f.write(outputs[0])
        else:
            self.compare(expected_file, outputs[0])


for exercise, case in cases():
    setattr(ExerciseTest, re.sub(r"\W", "_", "test_{}_{}".format(exercise, case)),
            lambda self, exercise=exercise, case=case: self.check_case(exercise, case))


if __name__ == "__main__":
    unittest.main()
