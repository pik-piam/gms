# update_fulldataOutput

Creates GAMS code which stores automatically the levels and marginals of
all equations and variables in time depending parameters.

## Usage

``` r
update_fulldataOutput(
  modelpath = ".",
  modulepath = "modules",
  corepath = "core",
  loopset = "t"
)
```

## Arguments

- modelpath:

  Path of the Model version that should be updated (main folder).

- modulepath:

  Module path within the model (relative to the model main folder)

- corepath:

  Core path within the model (relative to the model main folder)

- loopset:

  Set over which loop runs

## See also

[`fulldataOutput`](fulldataOutput.md),[`replace_in_file`](replace_in_file.md)

## Author

Jan Philipp Dietrich, Felicitas Beier
