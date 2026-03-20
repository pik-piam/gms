# getfiledestinations

Create file2destination mapping based on information from the model,
ignoring top-level directories listed in `.gitignore`.

## Usage

``` r
getfiledestinations(path = ".", ignoreFolders = "renv")
```

## Arguments

- path:

  main model folder

- ignoreFolders:

  folders to be ignored by the function, additionally to directories
  listed in `.gitignore` (by default only "renv").

## Author

Jan Philipp Dietrich, David Klein
