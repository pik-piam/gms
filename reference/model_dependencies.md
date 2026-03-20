# Function to detect R package dependencies

This function analyzes a model folder and all subfolders and searches
for library and require statements.

## Usage

``` r
model_dependencies(mainfolder = ".")
```

## Arguments

- mainfolder:

  main folder of the model to be analyzed

## Value

A list of dependencies sorted by appearances

## Author

Jan Philipp Dietrich
