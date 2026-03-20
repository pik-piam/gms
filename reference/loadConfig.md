# Load Config

Load config in YAML format as written via [`saveConfig`](saveConfig.md).

## Usage

``` r
loadConfig(cfg)
```

## Arguments

- cfg:

  Either a character string naming a file which containes the config or
  a character string containing the config as YAML code.

## Details

To read in the default configuration (stored as R list in default.cfg or
in a gams file), use [`readDefaultConfig`](readDefaultConfig.md)
instead.

## See also

[`saveConfig`](saveConfig.md)

## Author

Jan Philipp Dietrich

## Examples

``` r
cfg <- list(input = c(data1 = "blub.tgz", data2 = "bla.tgz"), mode = "default")
yml <- saveConfig(cfg)
loadConfig(yml)
#> $input
#>      data1      data2 
#> "blub.tgz"  "bla.tgz" 
#> 
#> $mode
#> [1] "default"
#> 
```
