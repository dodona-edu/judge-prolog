#!/bin/bash

# process arguments
RUNNER_SCRIPT=$1
LOG_PATH=$2

# kill all child processes on exit
trap "pkill -P $$" EXIT

# start memory footprint logging
[ -f /logger.sh ] && /logger.sh "$LOG_PATH" &

# switch to user "runner" and start the script
su runner -c "PATH='$PATH' ${RUNNER_SCRIPT}"
