# checkAppearance

Checks for all declared objects in which parts of the model they appear
and calculates the type of each object (core object, interface object,
module object of module xy,...)

## Usage

``` r
checkAppearance(x, capitalExclusionList = NULL)
```

## Arguments

- x:

  A code list as returned by [`codeExtract`](codeExtract.md)

- capitalExclusionList:

  A vector of names that should be ignored when checking for unified
  capitalization of variables

## Value

A list with four elements: appearance, setappearance, type and warnings.
Appearance is a matrix containing values which indicate whether an
object appears in a part of the code or not (e.g. indicates whether
"vm_example" appears in realization "on" of module "test" or not.). 0
means that it does not appear, 1 means that it appears in the code and 2
means that it appears in the not_used.txt. setappearance contains the
same information but for sets instead of other objects. Type is a vector
containing the type of each object (exluding sets). And warnings
contains a list of warnings created during that process.

## See also

[`codeCheck`](codeCheck.md),[`readDeclarations`](readDeclarations.md)

## Author

Jan Philipp Dietrich
