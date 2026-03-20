# path

Small function to build a consistent path-string based on folder,
filename and filetype. The function makes sure that slashes and the dot
for the file ending are set correctly (you can supply your folder name
either with or without a tailing slash in it. It does not matter.

## Usage

``` r
path(..., ftype = NULL)
```

## Arguments

- ...:

  the folders and the file name that should be pasted to a file/folder
  path

- ftype:

  file type

## Value

A string containing the path combined of folder, filename and filetype

## Author

Jan Philipp Dietrich
