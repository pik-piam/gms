# readParameterValues

Read values of given parameters from a given GAMS code section.

## Usage

``` r
readParameterValues(code, parameters)
```

## Arguments

- code:

  A vector with GAMS code.

- parameters:

  A vector with GAMS parameter names

## Value

A vector of values the parameters are set to with parameter names as
names.

## Details

The GAMS code section should contain statements of the form parameter =
value; for all the given parameters.

## See also

[`readDeclarations`](readDeclarations.md)

## Author

Mika Pflüger
