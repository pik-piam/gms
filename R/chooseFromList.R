#' chooseFromList
#'
#' Allows the user to select single or multiple items from a list.
#' Entries can be selected based on individual choice, groups, regex or all.
#'
#' @param theList list or character vector to be selected from, names can specify groups
#' @param type string in plural shown to user to understand what they have to choose
#' @param userinfo string printed to the user before choosing
#' @param addAllPattern boolean whether 'all' and 'Search by pattern' options are added
#' @param returnBoolean TRUE: returns array with dimension of theList with FALSE and TRUE,
#'                      which erases the order in which entries were selected
#'                      FALSE: returns selected entries of theList, conserving the order
#'                      in which entries were selected
#' @param multiple TRUE: allows to select multiple entries. FALSE: no
#' @param userinput string provided by the user. If not supplied, user is asked (mainly for testing)
#'
#' @return list or character vector, either a boolean with same length as theList or only the selected items.
#'
#' @examples
#'
#'  \dontrun{
#'     chooseFromList(
#'       theList = c(Letter = "A", Letter = "B", Number = "1", Number = "2"),
#'       type = "characters",
#'       userinfo = "Please don't select B, it hurts.",
#'       returnBoolean = FALSE,
#'       multiple = TRUE)
#'   }
#'
#' @author Oliver Richters
#'
#' @importFrom stringr str_pad
#'
#' @export
chooseFromList <- function(theList, type = "items", userinfo = NULL, addAllPattern = TRUE,
                           returnBoolean = FALSE, multiple = TRUE, userinput = FALSE) {
  originalList <- theList
  booleanList <- rep(FALSE, length(originalList)) # set to FALSE
  if (is.list(theList)) booleanList <- as.list(booleanList)
  m <- paste0("\n\nPlease choose ", type, ":\n\n")
  # paste group after each entry and add them to theList as options
  groups <- sort(unique(names(theList)[nchar(names(theList)) > 0]))
  if (multiple) {
    groupsids <- NULL
    if (length(groups) > 0) {
      groupsids <- seq(length(originalList) + 1 + 1 * addAllPattern,
                       length(originalList) + length(groups) + 1 * addAllPattern)
      theList <- c(paste0(str_pad(theList, max(nchar(originalList), 10), side = "right"), " ", names(theList)),
                   paste("Group:", groups))
      m <- c(m, paste0(str_pad("", max(nchar(originalList), 10) + nchar(length(theList) + 2) + 2, side = "right"),
                       " Group\n"))
    }
    # add all and regex pattern as options
    if (addAllPattern) theList <- c("all", theList, "Search by pattern...")
  }
  m <- c(m, paste(paste(str_pad(seq_along(theList), nchar(length(theList)), side = "left"),
                        theList, sep = ": "),
                  collapse = "\n"))
  m <- c(m, "\n", userinfo,
            paste0("\nNumber", if (multiple) "s entered as 2,4:6,9", " or leave empty:"))
  if (isFALSE(userinput)) { # print options and ask for userinput
    message(m)
    userinput <- getLine()
  }
  # interpret userinput and perform basic checks
  identifier <- try(eval(parse(text = paste("c(", userinput, ")"))))
  if (! all(grepl("^[0-9,: ]*$", userinput)) || inherits(identifier, "try-error")) {
    message("Try again, you have to choose some numbers.")
    return(chooseFromList(originalList, type, userinfo, addAllPattern, returnBoolean, multiple))
  }
  # check whether all input is usable
  if (! multiple && length(identifier) > 1) {
    message("Try again, multiple chosen: ", paste(identifier, collapse = ", "))
    return(chooseFromList(originalList, type, userinfo, addAllPattern, returnBoolean, multiple))
  }
  if (any(! identifier %in% seq_along(theList))) {
    message("Try again, not all in list: ", paste(identifier, collapse = ", "))
    return(chooseFromList(originalList, type, userinfo, addAllPattern, returnBoolean, multiple))
  }
  if (multiple) {
    if (addAllPattern && any(identifier == "1")) { # all
      identifier <- seq_along(originalList)
    } else {
      # interpret group inputs and select all group mem
      selectedGroups <- sub("^Group: ", "", theList[intersect(identifier, groupsids)])
      identifier <- unique(c(identifier[! identifier %in% groupsids],
                             which(names(originalList) %in% selectedGroups) + addAllPattern))
      # if search by pattern is selected, ask for pattern and interpret it
      if (addAllPattern && any(identifier == length(theList))) {
        patternid <- choosePatternFromList(originalList, type)
        identifier <- unique(c(patternid + 1, identifier[identifier < length(theList)]))
      }
      identifier <- identifier - 1 * addAllPattern # if addAllPattern = TRUE, '1' is 'all' option
    }
  }
  booleanList[identifier] <- TRUE
  message("Selected: ", paste(originalList[identifier], collapse = ", "))
  if (returnBoolean) return(booleanList) else return(originalList[identifier])
}

choosePatternFromList <- function(theList, type = "items", pattern = FALSE) {
  confirmchoice <- FALSE
  if (isFALSE(pattern)) {
    confirmchoice <- TRUE
    message("\nInsert the search pattern or the regular expression: ")
    pattern <- getLine()
  }
  id <- grep(pattern = pattern, theList)
  # lists all chosen and ask for the confirmation of the made choice
  if (length(id) > 0) {
    message("\n\nThe search pattern matches the following ", type, ":")
    message(paste(paste(seq_along(id), theList[id], sep = ": "), collapse = "\n"))
  } else {
    message("Oops. You didn't select anything.")
  }
  if (confirmchoice) {
    message("\nAre you sure these are the right ", type, "? (y/n): ")
    if (! getLine() %in% c("y", "Y")) {
      return(choosePatternFromList(theList, type))
    }
  }
  return(id)
}
