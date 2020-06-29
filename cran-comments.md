## Test environments
* local R installation, R 3.6.3
* ubuntu 16.04 (on travis-ci), R 3.6.3
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release to CRAN.
* Package provides tools to deal with modular code written in GAMS (http://gams.com)

* Updated code based on remarks by Swetlana Herbrandt (thanks again for checking!):

"Thanks, please write package names, software names and API names in single quotes (e.g. 'GAMS') in Title and Description."
DONE!

"Please add more details about the package functionality in your Description text."
DONE!

"Will your package work without 'GAMS'?"
Yes, it does. An installation of GAMS is not required.

"Please replace cat() by message() or warning() in your functions (except for print() and summary() functions). Messages and warnings can be suppressed if needed.""
DONE!

"Please ensure that your functions do not modify (save or delete) the user's home filespace in your examples/vignettes/tests. That is not allow by CRAN policies. Please only write/save files if the user has specified a directory. In your examples/vignettes/tests you can write to tempdir()."
I found a test which was under certain circumstances writing a pdf to the current, active directory. I modified the code so that it now writes this file to tempdir().

"Please ensure that your examples are executable. Use system.file() to find the correct package path."
I checked all examples again and performed modifications where it was necessary.
