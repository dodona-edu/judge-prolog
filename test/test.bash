#!/usr/bin/env bash
set -ex 
DIR="$(dirname $0 | xargs readlink -m)"
EXDIR="$(dirname $0 | xargs readlink -m)"
rm -rf /tmp/dodona-test
mkdir -p /tmp/dodona-test/workdir
if [ -z "$2" ]; then
cp $(find "$1/solution" -iname "*.pl" | head -n1)  /tmp/dodona-test/submission.pl
else
cp $2 /tmp/dodona-test/submission.pl
fi
cp -r $1/* /tmp/dodona-test

$DIR/../run <<HERE
{
    "resources": "/tmp/dodona-test/evaluation",
    "judge": "$DIR/..",
    "workdir": "/tmp/dodona-test/workdir",
    "time_limit": 60,
    "memory_limit": 100000000,
    "source": "/tmp/dodona-test/submission.pl",
    "programming_language":"prolog"
}
HERE

