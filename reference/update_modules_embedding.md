# Update Modules Embedding in GAMS code

A function that updates in the GAMS code all include commands which are
related to Modules. The function automatically checks which modules
exist and which files in these modules exist and creates the
corresponding include commands in GAMS

## Usage

``` r
update_modules_embedding(
  modelpath = ".",
  modulepath = "modules/",
  includefile = "modules/include.gms",
  verbose = FALSE
)
```

## Arguments

- modelpath:

  Path to the model that should be updated (main folder).

- modulepath:

  Module path within the model (relative to the model main folder)

- includefile:

  Name and location of the file which includes all modules (relative to
  main folder)

- verbose:

  Defines whether additional information should be printed or not.

## Note

Module phases are automatically detected checking the main code of the
model, but not checking code in modules. If you want to use additional
phases which are only included within a module, you need to specify them
manually by adding a comment into your gams code indicating that there
is an additional phase. The syntax is "\* !add_phase!: \<phase\>", e.g.
"\* !add_phase!: new_phase"

## Author

Jan Philipp Dietrich

## Examples

``` r
# copy dummymodel to temporary directory and update module embedding
file.copy(system.file("dummymodel", package="gms"), tempdir(), recursive = TRUE)
#> [1] TRUE
model   <- paste0(tempdir(),"/dummymodel")
update_modules_embedding(model)
```
