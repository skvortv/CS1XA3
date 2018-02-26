#!/bin/bash

#q1

s= git status
echo $s

#q2

git diff > changes.log
echo "All unncommited changes are in changes.log"
#q3

grep -h -r "#TODO" --exclude="ProjectAnalyze.sh" --exclude="todo.log" --exclude="changes.log"  > todo.log
echo "All lines with #TODO are in todo.log"

#q4
shopt -s globstar
for i in **/*.hs; do
    if grep -q main= $i
    then
    :
    else
    echo "main=undefined" >> $i
    fi
done

>error.log
for i in **/*.hs; do
	  ghc -fno-code $i &>> error.log
done
	

