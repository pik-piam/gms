# chooseFromList

Allows the user to select single or multiple items from a list. Entries
can be selected based on individual choice, groups, regex or all.

## Usage

``` r
chooseFromList(
  theList,
  type = "items",
  userinfo = NULL,
  addAllPattern = TRUE,
  returnBoolean = FALSE,
  multiple = TRUE,
  userinput = FALSE,
  errormessage = NULL
)
```

## Arguments

- theList:

  list or character vector to be selected from, names can specify groups

- type:

  string in plural shown to user to understand what they have to choose

- userinfo:

  string printed to the user before choosing

- addAllPattern:

  boolean whether 'all' and 'Search by pattern' options are added

- returnBoolean:

  TRUE: returns array with dimension of theList with FALSE and TRUE,
  which erases the order in which entries were selected FALSE: returns
  selected entries of theList, conserving the order in which entries
  were selected

- multiple:

  TRUE: allows to select multiple entries. FALSE: no

- userinput:

  string provided by the user. If not supplied, user is asked (mainly
  for testing)

- errormessage:

  string used internally to tell the user before retrying that a
  selection does not make sense

## Value

list or character vector, either a boolean with same length as theList
or only the selected items.

## Author

Oliver Richters

## Examples

``` r
 if (FALSE) { # \dontrun{
    chooseFromList(
      theList = c(Letter = "A", Letter = "B", Number = "1", Number = "2"),
      type = "characters",
      userinfo = "Please don't select B, it hurts.",
      returnBoolean = FALSE,
      multiple = TRUE)
  } # }
```
