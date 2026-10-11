#!/usr/bin/env bash
# Rewrites tests/fixtures: runs the judge on the cases below inside the judge
# image and keeps the raw output of one swipl run per case.
#
#   IMAGE=...   judge image (default ghcr.io/dodona-edu/dodona-prolog:latest)

set -euo pipefail

repo="$(cd "$(dirname "$0")/.." && pwd)"

docker run --rm --interactive --platform linux/amd64 --user root \
  --env PYTHONDONTWRITEBYTECODE=1 \
  --volume "$repo:/judge-prolog" \
  "${IMAGE:-ghcr.io/dodona-edu/dodona-prolog:latest}" \
  python3 - <<'EOF'
import json
import os
import shutil
import subprocess
import tempfile

REPO = "/judge-prolog"
EXERCISES = os.path.join(REPO, "tests", "exercises")
FIXTURES = os.path.join(REPO, "tests", "fixtures")
# A fixed sandbox, so the paths in the fixtures don't change between captures.
SANDBOX = "/mnt/sandbox"

# Fixture, exercise, submission (a file in submissions/ or the source itself),
# text that identifies the swipl run in its arguments or script, files to keep.
CASES = [
    ("plunit/correct_realhug", "plunit-family", "correct.pl",
     "begin_tests(realhug)", ["stderr"]),
    ("plunit/assertion_failure_brohug", "plunit-family", "assertion_failure.pl",
     "begin_tests(brohug)", ["stderr"]),
    ("plunit/syntax_error_realhug", "plunit-family", "syntax_error.pl",
     "begin_tests(realhug)", ["stderr"]),
    ("quickcheck/correct_prop_reverse_twice", "quickcheck-reverse", "correct.pl",
     "quickcheck(prop_reverse_twice/1)", ["result.json"]),
    ("quickcheck/wrong_prop_reverse_twice", "quickcheck-reverse", "wrong.pl",
     "quickcheck(prop_reverse_twice/1)", ["result.json"]),
    ("quickcheck/arithmetic_error", "quickcheck-error", "error.pl",
     "quickcheck(prop_double/1)", ["stderr"]),
    ("quickcheck/syntax_error", "quickcheck-error", "double(X, Y) :- Y is 2 X.\n",
     "quickcheck(prop_double/1)", ["stderr"]),
    ("simpltest/echo_output", "simple-echo", "output.pl",
     "dodonacheck:dotests()", ["stdout"]),
    ("simpltest/consult_syntax_error", "simple-consult", "stuff(3, 4) stuff.\n",
     "dodonacheck:dotests()", ["stderr"]),
    ("formcheck/singleton", "formcheck-grandparent", "singleton.pl",
     "'dodona lint'", ["stderr"]),
    ("formcheck/discontiguous", "formcheck-grandparent", "discontiguous.pl",
     "'dodona lint'", ["stderr"]),
    ("formcheck/undefined", "formcheck-grandparent", "undefined.pl",
     "'dodona lint'", ["result.json"]),
]

# Runs the real swipl and keeps its arguments, script, streams and result.json.
SWIPL = r"""#!/bin/bash
run=$(mktemp -d "$CAPTURE_DIR/XXXXXX")
printf '%s\n' "$@" > "$run/args"
cp "${@: -1}" "$run/script.pl"
/usr/bin/swipl "$@" > >(tee "$run/stdout") 2> >(tee "$run/stderr" >&2)
status=$?
cp result.json "$run/result.json" 2>/dev/null
exit $status
"""


def run_judge(exercise, submission, bin_dir, capture_dir):
    exercise_dir = os.path.join(EXERCISES, exercise)
    resources = os.path.join(SANDBOX, "resources")
    source = os.path.join(SANDBOX, "submission", "source")
    workdir = os.path.join(tempfile.gettempdir(), "workdir")
    for path in (resources, os.path.dirname(source), workdir):
        shutil.rmtree(path, ignore_errors=True)
    shutil.copytree(os.path.join(exercise_dir, "evaluation"), resources)
    os.makedirs(os.path.dirname(source))
    if "\n" in submission:
        with open(source, "w") as f:
            f.write(submission)
    else:
        shutil.copyfile(os.path.join(exercise_dir, "submissions", submission), source)
    if os.path.isdir(os.path.join(exercise_dir, "workdir")):
        shutil.copytree(os.path.join(exercise_dir, "workdir"), workdir)
    else:
        os.mkdir(workdir)

    config = {"time_limit": 10, "memory_limit": 536870912}
    if os.path.exists(os.path.join(exercise_dir, "config.json")):
        with open(os.path.join(exercise_dir, "config.json")) as f:
            config.update(json.load(f))
    config.update({
        "programming_language": "prolog",
        "natural_language": "en",
        "judge": os.path.join(SANDBOX, "judge"),
        "random_seed": 1,
        "resources": resources,
        "source": source,
        "workdir": workdir,
    })
    env = dict(os.environ, PATH=bin_dir + ":" + os.environ["PATH"], CAPTURE_DIR=capture_dir)
    subprocess.run(["/main.sh", os.path.join(SANDBOX, "judge", "run")], input=json.dumps(config),
                   capture_output=True, text=True, cwd=workdir, env=env, check=True)


def find_run(capture_dir, marker):
    runs = []
    for run in os.listdir(capture_dir):
        with open(os.path.join(capture_dir, run, "args")) as f:
            args = f.read()
        with open(os.path.join(capture_dir, run, "script.pl")) as f:
            script = f.read()
        if marker in args or marker in script:
            runs.append(os.path.join(capture_dir, run))
    if len(runs) != 1:
        raise RuntimeError("{} swipl runs match {}".format(len(runs), marker))
    return runs[0]


shutil.rmtree(SANDBOX, ignore_errors=True)
shutil.copytree(REPO, os.path.join(SANDBOX, "judge"),
                ignore=shutil.ignore_patterns(".git", ".claude", "tests", "__pycache__"))
bin_dir = tempfile.mkdtemp()
with open(os.path.join(bin_dir, "swipl"), "w") as f:
    f.write(SWIPL)
os.chmod(os.path.join(bin_dir, "swipl"), 0o755)

shutil.rmtree(FIXTURES, ignore_errors=True)
for fixture, exercise, submission, marker, keep in CASES:
    capture_dir = tempfile.mkdtemp()
    run_judge(exercise, submission, bin_dir, capture_dir)
    run = find_run(capture_dir, marker)
    for name in keep:
        path = os.path.join(run, name)
        if not os.path.exists(path) or os.path.getsize(path) == 0:
            raise RuntimeError("{}: no {}".format(fixture, name))
        target = os.path.join(FIXTURES, "{}.{}".format(fixture, name))
        os.makedirs(os.path.dirname(target), exist_ok=True)
        shutil.copyfile(path, target)
    unused = [name for name in ("stdout", "stderr", "result.json")
              if name not in keep and os.path.exists(os.path.join(run, name))
              and os.path.getsize(os.path.join(run, name)) > 0]
    print(fixture, "kept", keep, "left out", unused)
EOF
