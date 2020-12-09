#!/bin/bash

GIT_DIR=${1:-./}
GIT_PAST=${2:-6}
GIT_MAX=${3:-5}
GIT_MIN=${4:-1}


(cd $GIT_DIR ;git log --date=raw |grep Date: ) | ./day-grad.sh git $GIT_PAST $GIT_MAX $GIT_MIN

