# Save Config

Saves config in YAML format so that it can be read in again via
[`loadConfig`](loadConfig.md).

## Usage

``` r
saveConfig(cfg, file = NULL)
```

## Arguments

- cfg:

  Input config which should be saved

- file:

  A character string naming a file. If set to NULL the YAML output will
  be returned by the function.

## See also

[`loadConfig`](loadConfig.md)

## Author

Jan Philipp Dietrich

## Examples

``` r
cfg <- list(input = c(data1 = "blub.tgz", data2 = "bla.tgz"), mode = "default")
saveConfig(cfg)
#> [1] "input: !<namedVector>\n  data1: blub.tgz\n  data2: bla.tgz\nmode: default\n"
```
