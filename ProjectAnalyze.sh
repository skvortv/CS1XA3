#!/bin/bash
#q1
s= git status
echo $s

#q2
git diff > changes.log
echo "All unncommited changes are in changes.log"
#q3
grep -h -r "#TODO" --exclude="ProjectAnalyze.sh" --exclude="todo.log"  > todo.log
echo "All lines with #TODO are in todo.log"

#q4






