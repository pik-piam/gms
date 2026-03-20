# Download and unpack compressed data from repositories

Downloads a list of tgz files from a list of repos and unpacks them. If
a file is no .tgz-file it will be only downloaded.

## Usage

``` r
download_unpack(
  input,
  targetdir = "input",
  repositories = NULL,
  debug = FALSE,
  unpack = TRUE,
  stopOnMissing = FALSE
)
```

## Arguments

- input:

  a vector of files to be downloaded or a cfg list with settings to be
  used (e.g. containing cfg\$input, cfg\$repositories). Settings in the
  config list will be overwritten by other arguments of this function if
  they are not set to NULL

- targetdir:

  directory the files should be downloaded and extracted to

- repositories:

  a list of repositories (please pay attention to the list format!) in
  which the files should be searched for. Files will be searched in all
  repositories until found, always starting with the first repository in
  the list. The argument must have the format of a named list with the
  url of the repository as name and a corresponding list of options such
  as username or password to access the repository as value. If no
  options are required the value has to be NULL. (e.g.
  list("ftp://my_pw_protected_server.de/data"=list(user="me",password=12345),
  "http://free_server.de/dat"=NULL))

- debug:

  switch for debug mode with additional diagnostic information

- unpack:

  if switched off the source files are purley downloaded

- stopOnMissing:

  Boolean indicating whether to stop if any file in `files` could not be
  downloaded. Off (`FALSE`) by default. Can either be defined as a
  single boolean, which then applies equally to all elements of `files`,
  or can be defined individually for each element in `files`.

## Value

Information about the download process in form of a data.frame with data
sets as row names and repositories (where it was downloaded from) and
corresponding md5sum as columns

## Author

Jan Philipp Dietrich
