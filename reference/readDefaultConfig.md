# readDefaultConfig

Reads the default configuration of the model. Uses default.cfg and the
model's main gms as the source as appropriate. To read a configuration
from YAML format, use [`loadConfig`](loadConfig.md) instead.

## Usage

``` r
readDefaultConfig(path = ".", mainfile = "main.gms")
```

## Arguments

- path:

  path of the main folder of the model

- mainfile:

  filename of main model file, defaults to 'main.gms'

## Value

A vector of parameter values and their names.

## See also

[`loadConfig`](loadConfig.md)

## Author

Mika Pflüger
