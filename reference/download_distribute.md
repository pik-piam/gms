# Download and unpack compressed data from repositories

Downloads a list of tgz files from a list of repos and unpacks them

## Usage

``` r
download_distribute(
  files,
  repositories = list(`/p/projects/rd3mod/inputdata/output` = NULL),
  modelfolder = ".",
  additionalDelete = NULL,
  debug = FALSE,
  stopOnMissing = FALSE
)
```

## Arguments

- files:

  a vector of files containing input data, if a file is no .tgz it will
  be downloaded and not unpacked

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

- modelfolder:

  main model folder

- additionalDelete:

  information which additional data should be deleted before new data
  are downloaded and distributed

- debug:

  switch for debug mode with additional diagnostic information

- stopOnMissing:

  Boolean passed along to [`download_unpack()`](download_unpack.md) to
  stop if any file in `files` could not be downloaded. Off (`FALSE`) by
  default.

## Value

Information about the download process in form of a data.frame with data
sets as row names and repositories (where it was downloaded from) and
corresponding md5sum as columns

## Author

Jan Philipp Dietrich, Lavinia Baumstark
