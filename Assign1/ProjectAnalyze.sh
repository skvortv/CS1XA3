#!/bin/bash

#feature 1, Informs you if you're local repo is up to date with the remote repo

s= git status
echo $s

#feature 2, Puts all uncommited changes in a file changes.log

git diff > changes.log
echo "All uncommited changes are in changes.log"

#feature 3, Puts each line from every file of your project with the tag #TODO into a file todo.log

grep -h -r "#TODO" --exclude="ProjectAnalyze.sh" --exclude="todo.log" --exclude="changes.log"  > todo.log
echo "All lines with #TODO are in todo.log"


#feature 4, Checks all haskell files for syntax errors and puts the results into error.log
 
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
	  ghc -fno-code $i >> error.log 2>&1
done
echo "all haskell files with syntax errors are in error.log"

#feature 5, checks all python files for syntax errors (Rubinstd)

echo "" >error_python.log
find . -name "*.py" -type f | while read line; do
	echo "Errors for \"$line\" :" >>error_python.log
	echo "" >>error_python.log
	python -m py_compile "$line"&>>error_python.log
	echo "" >>error_python.log
done
			
#feature 6, finds a file that the user wished to see and moves it to the user's current directory (deleeuwj1) 

function move () {
    read -p "Enter the name of the file you wish to find: " fileName
    if [ $(find . -name "$fileName" -type f | wc -l) -gt 0 ] #looks through all sub-directories and moves the file to the current directory
    then
        fileLocation=$(find `pwd` -name $fileName) 
        cp -v $fileLocation .  #moves the file from it's location into the current directory
        echo "Your file in now in $PWD"
    else
        echo "$fileName does not exist"
    fi 
}

#feature 7,

read -p "would you like to work on a file with a #TODO line? (Y/N)" choice
case "$choice" in 
  y|Y )	vim "$(grep -r -h -l -m 1 "#TODO" --exclude="ProjectAnalyze.sh" --exclude="todo.log" --exclude="changes.log" )";;
  * ) echo "";;
esac


