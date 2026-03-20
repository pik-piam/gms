# Create tgz archive from directory

Creates a tgz from all files in a directory

## Usage

``` r
tardir(dir = ".", tarfile = "data.tgz")
```

## Arguments

- dir:

  directory from which the tar file should be generated

- tarfile:

  name of the archive the data should be written to (tgz file)

## Author

Jan Philipp Dietrich

## Examples

``` r
# copy dummymodel to temporary directory and compress it
file.copy(system.file("dummymodel",package="gms"),tempdir(), recursive = TRUE)
#> [1] TRUE
model   <- paste0(tempdir(),"/dummymodel")
archive <- paste0(tempdir(),"/dummymodel.tgz")
tardir(model,archive)
```
