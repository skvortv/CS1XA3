Feature 1 

Informs you if you're local repo is up to date with the remote repo. This just does the git status command.

Feature 2

Puts all uncommited changes into a file changes.log. Also fairly simple just uses the git diff command and appends it to changes.log.

Feature 3

Puts each line from every file of your project with the tag #TODO into a file todo.log. It greps through all directories and puts all the lines in the todo file. It ignores the files ProjectAnalyze.sh, todo.log, changes.log, to avoid redundant #TODO 's.

Feature 4

Checks all haskell files for syntax errors and puts the results into error.log. Firstly, it goes through all .hs files and if the main is not defined. it appends "main=undefined" to avoid the main not defined error messages. Then it goes through all .hs files and uses the ghc -fno-code command and appends that to error.log.

Feature 5 

A cool feature made by Daniel Rubinstein which checks for all pytho files for syntax errors.

Feature 6 

Another cool feature made by Jessica de Leeuw which finds the user's file and moves it to the current directory. Useful to move files up directories easier.

Feature 7

A feature of my own design which saves you the trouble of finding your files with #TODO lines. It will find you a file with a #TODO line and prompt you if you want to start working on the file right now. It will open the file with vim if you want it to. The code for the promt was taken from ghostdog74 Stack overflow post. 

