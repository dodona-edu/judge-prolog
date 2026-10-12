#!/usr/bin/env bash
# Runs devel/run-tests.sh as root inside the judge image. Arguments are passed
# on, e.g. `-v -k echo`.
#
#   IMAGE=...           judge image (default ghcr.io/dodona-edu/dodona-prolog:latest)
#   UPDATE_EXPECTED=1   rewrite the expected output

set -euo pipefail

repo="$(cd "$(dirname "$0")/.." && pwd)"

docker run --rm --platform linux/amd64 --user root \
  ${UPDATE_EXPECTED:+--env UPDATE_EXPECTED} \
  --volume "$repo:/judge-prolog" \
  --workdir /judge-prolog \
  "${IMAGE:-ghcr.io/dodona-edu/dodona-prolog:latest}" \
  devel/run-tests.sh "$@"
