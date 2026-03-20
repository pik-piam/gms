# Merge GAMS code into single file

This function merges GAMS code which is distributed over severals files
into a single GAMS file. Optionally, it also embeds R scripts into the
single GAMS file

## Usage

``` r
singleGAMSfile(
  modelpath = ".",
  mainfile = "main.gms",
  output = "full.gms",
  embedRScripts = FALSE
)
```

## Arguments

- modelpath:

  The path where the model is stored

- mainfile:

  The path to the main gams file (relative to the model path)

- output:

  Name of the single output GAMS file.

- embedRScripts:

  If TRUE, R scripts called by GAMS via Execute are also embedded.
  Default FALSE

## Author

Jan Philipp Dietrich, Anastasis Giannousakis

## Examples

``` r
# copy dummymodel create single gms file out of it
file.copy(system.file("dummymodel", package = "gms"), tempdir(), recursive = TRUE)
#> [1] TRUE
model      <- paste0(tempdir(), "/dummymodel")
singlefile <- paste0(tempdir(), "/full.gms")
singleGAMSfile(modelpath = model, output = singlefile)
```
