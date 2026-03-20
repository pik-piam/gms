# Publish data in a repository

Downloads a list of tgz files from a list of repos, merge them and
publish it on another server

## Usage

``` r
publish_data(
  input,
  name = NULL,
  target = Sys.getenv("PUBLISH_DATA_TARGET", unset = "."),
  ...
)
```

## Arguments

- input:

  a vector of files to be downloaded or a cfg list with settings to be
  used (e.g. containing cfg\$input, cfg\$repositories). Settings in the
  config list will be overwritten by other arguments of this function if
  they are not set to NULL

- name:

  name of the data to be published (will be used in as file name). If no
  name is given (default) source files will be published as is (separate
  tgz files with original name).

- target:

  target the data should be published in (format
  user@server:/folder/path) If a target vector, or targets separated by
  "\|" are provided the user will be asked interactively where the file
  should be written to. By default it will look for target information
  in the environment variable PUBLISH_DATA_TARGET

- ...:

  further options provided to [`download_unpack`](download_unpack.md)

## See also

[`download_unpack`](download_unpack.md),[`tardir`](tardir.md)

## Author

Jan Philipp Dietrich
