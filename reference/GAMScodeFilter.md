# GAMScodeFilter

Cleans GAMS code supplied from empty lines and comments.

## Usage

``` r
GAMScodeFilter(x)
```

## Arguments

- x:

  A vector with lines of GAMS code (as you get by reading the code with
  readLines)

## Value

The cleaned GAMS code

## See also

[`readDeclarations`](readDeclarations.md)

## Author

Jan Philipp Dietrich

## Examples

``` r
GAMScodeFilter(c("","*comment","a=12;","","b=13;"))
#> [1] "a=12;" "b=13;"
```
