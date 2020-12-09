#!/bin/bash

GIT_DIR=${1:-./}
GIT_PAST=${2:-6}

(cd $GIT_DIR ;git log --date=raw |grep Date ) | ./day-grad.sh $GIT_PAST

