#!/usr/bin/env bash
if [ "$#" -ne "1" -a "$#" -ne "2" ]; then
	echo "Usage: $0 EXDIR [SOLFILE]"
	echo "  Will run the judge for the excersise with the given solution."
	echo "  If no SOLFILE is given soluion/solution.pl will be used"
	exit 1
fi

if [ \! -d "$1" ]; then
	echo "$1 is not a directory"
	exit 1
fi

set -e
DIR="$(dirname $0 | xargs readlink -m)"
rm -rf /tmp/dodona-test
mkdir -p /tmp/dodona-test/workdir
if [ -z "$2" ]; then
	if [ -e "$1/solution/solution.pl" ]; then
		cp "$1/solution/solution.pl" /tmp/dodona-test/submission.pl
	else
		cp $(find "$1/solution" -iname "*.pl" | head -n1) /tmp/dodona-test/submission.pl
	fi
else
	cp $2 /tmp/dodona-test/submission.pl
fi
cp -r $1/* /tmp/dodona-test

$DIR/../run <<HERE
{
    "resources": "/tmp/dodona-test/evaluation",
    "judge": "$DIR/..",
    "workdir": "/tmp/dodona-test/workdir/",
    "time_limit": 60,
    "memory_limit": 100000000,
    "source": "/tmp/dodona-test/submission.pl",
    "programming_language":"prolog"
}
HERE
