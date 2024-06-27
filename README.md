# 'GAMS' Modularization Support Package

R package **gms**, version **0.31.2**

[![CRAN status](https://www.r-pkg.org/badges/version/gms)](https://cran.r-project.org/package=gms) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.4390032.svg)](https://doi.org/10.5281/zenodo.4390032) [![R build status](https://github.com/pik-piam/gms/workflows/check/badge.svg)](https://github.com/pik-piam/gms/actions) [![codecov](https://codecov.io/gh/pik-piam/gms/branch/master/graph/badge.svg)](https://app.codecov.io/gh/pik-piam/gms) [![r-universe](https://pik-piam.r-universe.dev/badges/gms)](https://pik-piam.r-universe.dev/builds)

## Purpose and Functionality

A collection of tools to create, use and maintain modularized model code written in the modeling 
    language 'GAMS' (<https://www.gams.com/>). Out-of-the-box 'GAMS' does not come with support for modularized
    model code. This package provides the tools necessary to convert a standard 'GAMS' model to a modularized one
    by introducing a modularized code structure together with a naming convention which emulates local
    environments. In addition, this package provides tools to monitor the compliance of the model code with
    modular coding guidelines.


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

Dietrich J, Klein D, Giannousakis A, Beier F, Koch J, Baumstark L, Pflüger M, Richters O (2024). _gms: 'GAMS' Modularization Support Package_. doi: 10.5281/zenodo.4390032 (URL: https://doi.org/10.5281/zenodo.4390032), R package version 0.31.2, <URL: https://github.com/pik-piam/gms>.

A BibTeX entry for LaTeX users is

 ```latex
@Manual{,
  title = {gms: 'GAMS' Modularization Support Package},
  author = {Jan Philipp Dietrich and David Klein and Anastasis Giannousakis and Felicitas Beier and Johannes Koch and Lavinia Baumstark and Mika Pflüger and Oliver Richters},
  year = {2024},
  note = {R package version 0.31.2},
  doi = {10.5281/zenodo.4390032},
  url = {https://github.com/pik-piam/gms},
}
```
