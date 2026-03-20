# settingsCheck

Checks GAMS setglobals in code for consistency. Creates a warning if a
setglobal command for an existing module is missing or a module is set
to a realization which does not exist.

## Usage

``` r
settingsCheck(path = ".", modulepath = "modules", fileName = "main.gms")
```

## Arguments

- path:

  path of the main folder of the model

- modulepath:

  path to the module folder relative to "path"

- fileName:

  name of the file containing setglobals, relative to "path"

## Value

Nothing is returned.

## See also

[`codeCheck`](codeCheck.md)

## Author

Jan Philipp Dietrich
