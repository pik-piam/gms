# Check config

Checks a model configuration file for consistency by comparing it to a
reference config file and the given module structure of the model. The
function will throw out an error if settings are missing in the config
which exist in the reference config, of if settings are set in the
config which do not exist in the reference config file or if a
realization is chosen for a module which does not exist, not allowed
setting combinations.

## Usage

``` r
check_config(
  icfg,
  reference_file = "config/default.cfg",
  modulepath = "modules/",
  settings_config = NULL,
  extras = NULL,
  saveCheck = FALSE
)
```

## Arguments

- icfg:

  Input config which should be checked for consistency (either as the
  config itself or as a file path linking to the config)

- reference_file:

  Reference config which is having the right format (either as the
  config itself or as a file path linking to the config)

- modulepath:

  The path where the modules are stored. If set to NULL the
  corresponding module check is deactivated.

- settings_config:

  path where the table of possible setting combinations is stored, if
  NULL it is ignored

- extras:

  vector of setting names that are allowed to appear in the input config
  even if they are missing in the reference config. That can be useful
  to allow for additional settings/information which does not
  necessarily have to exist

- saveCheck:

  additional check which makes sure that saving and loading it to/from
  YAML format does not change its contents

## Value

The checked config as a config list ready for further usage.

## See also

[`getModules`](getModules.md)

## Author

Jan Philipp Dietrich, Lavinia Baumstark
