# checkDescription

Checks whether all Declarations of a GAMS code come with a Description,
throws out a warning in case of a missing description.

## Usage

``` r
checkDescription(x, w = NULL)
```

## Arguments

- x:

  GAMS declarations matrix as returned by

- w:

  a vector of warnings the warnings should be added to
  [`readDeclarations`](readDeclarations.md)

## Value

vector of warnings

## See also

[`codeCheck`](codeCheck.md)

## Author

Jan Philipp Dietrich
