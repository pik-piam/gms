# codeExtract

Returns aggregated and cleaned GAMS code together with declaration
matrix

## Usage

``` r
codeExtract(codeFiles, name)
```

## Arguments

- codeFiles:

  A vector of file names of GAMS code files.

- name:

  A name indicating what collection of code files this is (e.g. module
  name)

## Value

A list with two elements: code and declarations. Code contains the
cleaned up gams code and declarations contains the declarations matrix
as returned by [`readDeclarations`](readDeclarations.md)

## See also

[`codeCheck`](codeCheck.md),[`readDeclarations`](readDeclarations.md)

## Author

Jan Philipp Dietrich
