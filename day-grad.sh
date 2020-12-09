#!/bin/bash

ARGS=$@

JAR=`ls target/scala-2.13/day-grid-assembly-*.jar`

java -jar $JAR $ARGS >index.html

