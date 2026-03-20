# codeCheck

Checks GAMS code for consistency. Throws out warnings if something is
wrong in the code and returns a list containing the interfaces of each
module of the code.

## Usage

``` r
codeCheck(
  path = ".",
  modulepath = "modules",
  core_files = c("core/*.gms", "main.gms"),
  returnDebug = FALSE,
  interactive = FALSE,
  test_switches = TRUE,
  strict = FALSE,
  details = FALSE
)
```

## Arguments

- path:

  path of the main folder of the model

- modulepath:

  path to the module folder relative to "path"

- core_files:

  list of files that belong to the core (wildcard expansion is
  supported)

- returnDebug:

  If TRUE additional information will be returned useful for debugging
  the codeCheck function

- interactive:

  activates an interactive developer mode in which some of the warnings
  can be fixed interactively.

- test_switches:

  (boolean) Should realization switches in model core be tested for
  completeness? Usually set to TRUE but should be set to FALSE for
  standalone models only using a subset of existing modules

- strict:

  (boolean) test strictness. If set to TRUE warnings from codeCheck will
  stop calculations at the end of the analysis. Useful to enforce clean
  code.

- details:

  (boolean) If activated the function will return more detailed output.
  Besides interface information it will provide a table containing all
  declarations in the code, an appearance table listing the appearance
  of all objects in the code and information about the existing modules.
  The format is list(interfaceInfo,declarations,appearance,modulesInfo).
  This setting will be ignored when returnDebug is set to TRUE.

## Value

A list of all modules containing the interfaces for each module. Or more
detailed output if either `details` or `returnDebug` is set to TRUE.

## Details

Additional settings can be provided via a yaml file ".codeCheck" in the
main folder of the model. Currently supported settings are: -
capitalExclusionList: a list of names that should be ignored when
checking for unified capitalization of variables

## See also

[`codeExtract`](codeExtract.md),[`readDeclarations`](readDeclarations.md)

## Author

Jan Philipp Dietrich

## Examples

``` r
# check code consistency of dummy model
codeCheck(system.file("dummymodel", package = "gms"))
#> 
#>  Running codeCheck...
#>  Finished data collection...            (time elapsed:  0.022)
#>  Naming conventions check done...       (time elapsed:  0.023)
#>   Running checkAppearance...
#>   Start variable matching...            (time elapsed:  0.001)
#>   Finished variable matching...         (time elapsed:  0.001)
#>   Start var capitalization check...     (time elapsed:  0.002)
#>   Finished var capitalization check...  (time elapsed:  0.003)
#>  Investigated variable appearances...   (time elapsed:  0.026)
#>  Appearance and usage check done...     (time elapsed:  0.027)
#>  Switch Appearance check done...        (time elapsed:  0.028)
#>  Interface collection and check done... (time elapsed:  0.029)
#>  Input folder check done...             (time elapsed:  0.029)
#>  Description check done...              (time elapsed:   0.03)
#>  All codeCheck tests passed!
#> $core
#>         out 
#> "pm_global" 
#> 
#> $fancymodule
#>            in           out 
#>   "pm_global" "vm_exchange" 
#> 
#> $crazymodule
#>            in 
#> "vm_exchange" 
#> 
```
