#!/bin/bash
DIR="$(dirname $0 | xargs readlink -m)"
rm -rf $DIR/workdir/* 2> /dev/null
cp -r $DIR/ex/description/media/* $DIR/workdir 2> /dev/null
$DIR/../run <<HERE
{
    "resources": "$DIR/ex/evaluation",
    "judge": "$DIR/..",
    "workdir": "$DIR/workdir",
    "time_limit": 30,
    "memory_limit": 100000000,
    "source": "$DIR/ex/solution/input.pl",
    "programming_language":"prolog"
}
HERE