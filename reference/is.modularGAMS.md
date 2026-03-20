# is.modularGAMS

Checks whether a folder seems to contain modular GAMS code or not.

## Usage

``` r
is.modularGAMS(path = ".", version = FALSE, modulepath = "modules/")
```

## Arguments

- path:

  path to the main folder of the model

- version:

  if TRUE returns the version of the modular structure or FALSE,
  otherwise returns a boolean indicating whether it is modular or not.

- modulepath:

  Module path within the model (relative to the model main folder)

## See also

[`codeCheck`](codeCheck.md)

## Author

Jan Philipp Dietrich

## Examples

``` r
is.modularGAMS(system.file("dummymodel",package="gms"))
#> [1] TRUE
```
