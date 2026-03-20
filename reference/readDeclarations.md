# readDeclarations

Reads all declarations given in a GAMS code and returns them.

## Usage

``` r
readDeclarations(
  file,
  unlist = TRUE,
  types = c("scalar", "(positive |negative |)variable", "parameter", "table", "equation",
    "set")
)
```

## Arguments

- file:

  A gams file or a vector containing GAMS code.

- unlist:

  A logical indicating whether the output should be returned as a list
  separated by object type or a matrix.

- types:

  of declarations to be read.

## Value

Either a list of declared objects or a matrix containing object name,
the sets the object depends on and the description.

## See also

[`codeCheck`](codeCheck.md)

## Author

Jan Philipp Dietrich
