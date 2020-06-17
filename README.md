# GAMS modularization support package

R package **gms**, version **0.4.0**

[![Travis build status](https://travis-ci.com/pik-piam/gms.svg?branch=master)](https://travis-ci.com/pik-piam/gms)  [![codecov](https://codecov.io/gh/pik-piam/gms/branch/master/graph/badge.svg)](https://codecov.io/gh/pik-piam/gms)

## Purpose and Functionality

A collection of tools to create, use and maintain modularized model code written in the modeling language GAMS (https://www.gams.com/).


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("gms")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Questions / Problems

In case of questions / problems please contact Jan Philipp Dietrich <dietrich@pik-potsdam.de>.

## Citation

To cite package **gms** in publications use:

Dietrich J, Klein D, Giannousakis A, Beier F, Koch J, Baumstark L (2020). _gms: GAMS modularization support package_. R package
version 0.4.0.

A BibTeX entry for LaTeX users is

 ```latex
@Manual{,
  title = {gms: GAMS modularization support package},
  author = {Jan Philipp Dietrich and David Klein and Anastasis Giannousakis and Felicitas Beier and Johannes Koch and Lavinia Baumstark},
  year = {2020},
  note = {R package version 0.4.0},
}
```

