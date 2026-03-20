# setScenario

setScenario is adapting a given config to a predefined scenario, meaning
that all settings which are fixed for the given scenario are written to
the config. Settings not defined by the scenario remain unchanged.

## Usage

``` r
setScenario(cfg, scenario, scenario_config = "config/scenario_config.csv")
```

## Arguments

- cfg:

  Input config which should be adapted to the given scenario

- scenario:

  name of scenario (e.g. "SSP2"). Can also be a vector of scenarios. In
  this case scenario settings are applied in the given order

- scenario_config:

  A data frame containing the scenario config table or the path where
  the scenario config table is stored.

## Value

The updated config as a config list ready for further usage.

## Note

The scenario config table is a table which contains as columns the
different scenarios and as rows the different settings. Empty entries
for a given scenario-setting combination indicate that this setting is
not defined by the scenario and should not be changed by set Scenario!

## See also

[`check_config`](check_config.md),[`getModules`](getModules.md)

## Author

Jan Philipp Dietrich, Anastasis Giannousakis
