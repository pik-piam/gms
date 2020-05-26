# Tools package for modular GAMS code

R package **gmod**, version **0.2.0**

[![Travis build status](https://travis-ci.com/pik-piam/gmod.svg?branch=master)](https://travis-ci.com/pik-piam/gmod)  [![codecov](https://codecov.io/gh/pik-piam/gmod/branch/master/graph/badge.svg)](https://codecov.io/gh/pik-piam/gmod)

## Purpose and Functionality

A collection of tools to create and use modularized GAMS code.


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("gmod")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Questions / Problems

In case of questions / problems please contact Jan Philipp Dietrich <dietrich@pik-potsdam.de>.

## Citation

To cite package **gmod** in publications use:

Dietrich J, Klein D, Giannousakis A, Beier F, Koch J, Baumstark L (2020). _gmod: Tools
package for modular GAMS code_. R package version 0.2.0.

A BibTeX entry for LaTeX users is

 ```latex
@Manual{,
  title = {gmod: Tools package for modular GAMS code},
  author = {Jan Philipp Dietrich and David Klein and Anastasis Giannousakis and Felicitas Beier and Johannes Koch and Lavinia Baumstark},
  year = {2020},
  note = {R package version 0.2.0},
}
```

