#!/usr/bin/env bash
if [ "$#" -ne "1" ]; then
	echo "usage $0 dir-with-excersises"
	echo "  will run all excersises in the folder with the judge"
    echo ""
    echo "  To run with docker set the env var"
    echo "  - WITH_DOCKER_IMAGE=specificimage"
    echo "  - WITH_DOCKER=1   (to run with dodona/dodona-prolog)"
	exit 1
fi

find "$1" -name "config.json" |
	sort |
	sed "s/config.json//" |
	while read line; do
		echo $line
		$(dirname $0)/test.bash "$line" | grep '^\s*"status":'
	done
