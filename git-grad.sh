#!/bin/bash

git log --date=raw |grep Date | java -jar target/scala-2.13/day-grid-assembly-0.0.1-SNAPSHOT.jar git

echo "Generated: index.html"
