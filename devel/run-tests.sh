#!/usr/bin/env bash
# Runs the test suite. Meant to run as root inside the judge image, like CI;
# devel/run-tests-in-image.sh does that locally. Arguments are passed to
# `python3 -m unittest discover`, e.g. `-v -k echo`.

set -euo pipefail

cd "$(dirname "$0")/.."

python3 -m unittest discover --start-directory tests "$@"
