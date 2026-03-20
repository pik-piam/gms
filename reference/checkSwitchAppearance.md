# checkSwitchAppearance

Checks for all declared switches in which parts of the model they appear
and calculates the type of each object (core object, interface object,
module object of module xy,...)

## Usage

``` r
checkSwitchAppearance(code)
```

## Arguments

- code:

  Model code returned by [`codeExtract`](codeExtract.md)

## Value

A list with three elements: switches, appearance and type. Switches is a
vector containing all switches. The names of the vector contain the
information where the switch is set. Appearance is a matrix containing
values which indicate whether an object appears in a part of the code or
not (e.g. indicates whether "vm_example" appears in realization "on" of
module "test" or not.). 0 means that it does not appear, 1 means that it
appears in the code and 2 means that it appears in the not_used.txt.
Type is a vector containing the type of each object.

## See also

[`codeCheck`](codeCheck.md),[`readDeclarations`](readDeclarations.md),[`codeExtract`](codeExtract.md),[`checkAppearance`](checkAppearance.md)

## Author

Jan Philipp Dietrich
