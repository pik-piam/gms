## Test environments
* local R installation, R 3.6.3
* ubuntu 16.04 (on travis-ci), R 3.6.3
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release to CRAN.
* Package provides tools to deal with modular code written in GAMS (http://gams.com)

* Updated code based on remarks by Swetlana Herbrandt (thanks again for checking!):

"Thanks, please replace \dontrun{} by \donttest{} or unwap the examples if they can be executed in less than 5 sec per Rd-file.
Please add more small executable examples in your Rd-files. Rd-file examples should help the use to understand how your functions work."

I updated the examples so that they now all run in a temporary directory, if they write/modify files. In addition, I added further examples where it seemed appropriate.