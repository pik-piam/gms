# Create a Module skeleton

This function creates you a module skeleton which you can use to easily
create your own modules.

## Usage

``` r
module.skeleton(
  number,
  name,
  types,
  modelpath = ".",
  modulepath = "modules/",
  includefile = "modules/include.gms",
  version = is.modularGAMS(modelpath, version = TRUE)
)
```

## Arguments

- number:

  Number of your module, typically something between 0-99. Sorts the
  execution of your modules. Please use a number which is not used, yet.

- name:

  Name of your module (please choose a short name). If you want to
  extend an existing module (add a new realisation) use the name of the
  existing one.

- types:

  Vector of names for the different module types (e.g. "on" or "off").
  If you want to extend an existing module (add a new realisation), put
  here the additional type(s)

- modelpath:

  Path of the MAgPIE version that should be updated (main folder).

- modulepath:

  Module path within MAgPIE (relative to the MAgPIE main folder)

- includefile:

  Name and location of the file which includes all modules (relative to
  main folder)

- version:

  version of the modular GAMS code structure (1 or 2)

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
# copy dummymodel to temporary directory and add new module "bla"
file.copy(system.file("dummymodel",package="gms"),tempdir(), recursive = TRUE)
#> [1] TRUE
model   <- paste0(tempdir(),"/dummymodel")
module.skeleton(number="03", name="bla", types=c("on","off"), modelpath=model)
```
