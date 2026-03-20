# fulldataOutput

Creates GAMS code which stores automatically the levels, bounds and
marginals of all equations and variables in time depending parameters.

## Usage

``` r
fulldataOutput(
  declarations_file = "declarations.gms",
  definitions_file = "postsolve.gms",
  warn = TRUE,
  types = c("level", "marginal"),
  ignore = "_dummy$",
  loopset = "t"
)
```

## Arguments

- declarations_file:

  A GAMS file containing declarations. The function will read
  declarations from here and add own declarations in an R environment as
  used by [`replace_in_file`](replace_in_file.md) (used subject = OUTPUT
  DECLARATIONS)

- definitions_file:

  A GAMS file which is executed after the solve statement but within the
  time step loop. Also here code will be added using
  [`replace_in_file`](replace_in_file.md) with subject OUTPUT
  DEFINITIONS

- warn:

  Decides whether a warning should be thrown out, if the declarations
  file does not exist.

- types:

  Types of outputs that should be written to gdx file. Available types
  are level, marginal, upper and lower.

- ignore:

  regular expression pattern for variables/equations which should be
  ignored by fulldataOutput

- loopset:

  Set over which loop runs

## See also

[`readDeclarations`](readDeclarations.md),[`replace_in_file`](replace_in_file.md)

## Author

Jan Philipp Dietrich, Felicitas Beier
