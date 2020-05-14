# Code Manipulation and Analysis Tools

R package **lucode**, version **3.20.4**

[![Travis build status](https://travis-ci.com/pik-piam/lucode.svg?branch=master)](https://travis-ci.com/pik-piam/lucode) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1158596.svg)](https://doi.org/10.5281/zenodo.1158596) [![codecov](https://codecov.io/gh/pik-piam/lucode/branch/master/graph/badge.svg)](https://codecov.io/gh/pik-piam/lucode)

## Purpose and Functionality

A collection of tools which allow to manipulate and analyze code.


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("lucode")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Questions / Problems

In case of questions / problems please contact Jan Philipp Dietrich <dietrich@pik-potsdam.de>.

## Citation

To cite package **lucode** in publications use:

Dietrich J, Klein D, Giannousakis A, Baumstark L, Bonsch M, Bodirsky B, Beier F, Koch J (2020). _lucode: Code
Manipulation and Analysis Tools_. doi: 10.5281/zenodo.1158596 (URL: https://doi.org/10.5281/zenodo.1158596), R
package version 3.20.4, <URL: https://github.com/pik-piam/lucode>.

A BibTeX entry for LaTeX users is

 ```latex
@Manual{,
  title = {lucode: Code Manipulation and Analysis Tools},
  author = {Jan Philipp Dietrich and David Klein and Anastasis Giannousakis and Lavinia Baumstark and Markus Bonsch and Benjamin Leon Bodirsky and Felicitas Beier and Johannes Koch},
  year = {2020},
  note = {R package version 3.20.4},
  doi = {10.5281/zenodo.1158596},
  url = {https://github.com/pik-piam/lucode},
}
```

