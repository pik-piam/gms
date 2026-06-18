# readSetItems

Parses the sets for a given code file and stores them in a named list.

## Usage

``` r
readSetItems(codeFile, warn = FALSE, name = NULL)
```

## Arguments

- codeFile:

  a GAMS code file

- warn:

  A boolean indicating if warnings should be displayed when parsing a
  sections fails

- name:

  A name indicating what collection of code files this is (e.g. module
  name). Only needed for more expressive warnings.

## Author

Falk Benke
