# modules_interfaceplot

Function that applies [`interfaceplot`](interfaceplot.md) for a whole
model and all its modules.

## Usage

``` r
modules_interfaceplot(
  x = ".",
  modulepath = "modules",
  filetype = "png",
  targetfolder = NULL,
  writetable = TRUE,
  includeCore = FALSE,
  ...
)
```

## Arguments

- x:

  Either the object returned by [`codeCheck`](codeCheck.md) or the path
  to the main folder of the model.

- modulepath:

  Path to the modules folder

- filetype:

  Filetype that should be used (e.g. "png" or "pdf")

- targetfolder:

  Folder outputs should be written to. If set to NULL outputs will be
  added to corresponding module folders

- writetable:

  Logical deciding whether a csv containing the interfaces should be
  written as well.

- includeCore:

  Logical to create plot for core or not, default FALSE.

- ...:

  Optional arguments to [`interfaceplot`](interfaceplot.md).

## Value

A list with interface tables for each module

## See also

[`codeCheck`](codeCheck.md),[`interfaceplot`](interfaceplot.md)

## Author

Jan Philipp Dietrich
