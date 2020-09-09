#!/usr/bin/env bash
if [ "$#" -ne "1" -a "$#" -ne "2" ]; then
	echo "Usage: $0 EXDIR [SOLFILE]"
	echo "  Will run the judge for the excersise with the given solution."
	echo "  If no SOLFILE is given soluion/solution.pl will be used"
	echo ""
	echo "  To run with docker set the env var"
	echo "  - WITH_DOCKER_IMAGE=specificimage"
	echo "  - WITH_DOCKER=1   (to run with dodona/dodona-prolog)"
	echo "  - WAIT=1   (don't start the tests. To run tests execute          )"
	echo "             (cd /home/runner; cat /mnt/init.json | /mnt/judge/run )"
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

if [ -n "$WITH_DOCKER" -o -n "$WITH_DOCKER_IMAGE" ]; then
	mkdir /tmp/dodona-test/judge
	cp -r "$DIR/../"* /tmp/dodona-test/judge
	cat <<HERE >/tmp/dodona-test/init.json
{
    "resources": "/mnt/evaluation",
    "judge": "/mnt/judge",
    "workdir": "/home/runner/workdir/",
    "time_limit": 60,
    "memory_limit": 100000000,
    "source": "/mnt/submission.pl",
    "programming_language":"prolog"
}
HERE

	if [ -n "$WAIT" ]; then
		set -x
		docker run -it --rm \
			-v "/tmp/dodona-test/:/mnt:rw" \
			-v "/tmp/dodona-test/workdir/:/home/runner/workdir:rw" \
			${WITH_DOCKER_IMAGE:-dodona/dodona-prolog} \
			/bin/bash
	else
		set -x
		cat /tmp/dodona-test/init.json |
			docker run -i --rm \
				-v "/tmp/dodona-test/:/mnt:rw" \
				-v "/tmp/dodona-test/workdir/:/home/runner/workdir:rw" \
				${WITH_DOCKER_IMAGE:-dodona/dodona-prolog} \
				/main.sh /mnt/judge/run
	fi
else
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
fi
