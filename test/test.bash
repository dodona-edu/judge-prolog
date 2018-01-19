#!/bin/bash
DIR="$(dirname $0 | xargs readlink -m)"
$DIR/../run <<HERE
{
    "resources": "$DIR/ex/evaluation",
    "judge": "$DIR/..",
    "workdir": "$DIR/ex/workdir",
    "time_limit": 30,
    "memory_limit": 100000000,
    "source": "$DIR/ex/solution/input.pl",
    "programming_language":"prolog"
}
HERE

cd $DIR/..
find -iname "*.extended.pl" -exec rm "{}" \; ; rm test/ex/workdir/result.json
cd -