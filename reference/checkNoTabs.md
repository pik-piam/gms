# checkNoTabs

Check all files (also in subdirectories) matching the given pattern for
tabs. Will throw an error with a list of files where tabs were found if
any.

## Usage

``` r
checkNoTabs(pattern, exclude = NULL, excludeFolders = NULL)
```

## Arguments

- pattern:

  A regular expression. Only files matching this pattern will be checked
  for tabs.

- exclude:

  A regular expression. Files matching this pattern will never be
  checked.

- excludeFolders:

  Paths to folders that should not be checked.

## Value

Invisibly, the list of files that were checked.

## Author

Pascal Sauer

## Examples

``` r
if (FALSE) { # \dontrun{
gms::checkNoTabs(pattern = "\\.(R|Rprofile|gms|cfg|bib)$",
                 excludeFolders = c("output", "renv", ".git"))
gms::checkNoTabs(utils::glob2rx("*.R"))
} # }
```
