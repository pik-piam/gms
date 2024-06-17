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
#' @param errormessage string used internally to tell the user before retrying that a selection does not make sense
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
                           returnBoolean = FALSE, multiple = TRUE, userinput = FALSE, errormessage = NULL) {
  originalList <- theList
  if (length(theList) == 0) {
    message("No ", type, " found that might be selected, returning the empty list.")
    return(theList)
  }
  addAllPattern <- addAllPattern && multiple
  booleanList <- rep(FALSE, length(originalList)) # set to FALSE
  if (is.list(theList)) booleanList <- as.list(booleanList)
  m <- paste0("\n\nPlease choose ", type, ":\n\n")
  # paste groups after each entry and add them to theList as options
  rawNames <- names(theList)
  rawNames <- rawNames[nchar(rawNames) > 0]
  groups <- NULL
  if (length(rawNames) > 0) {
    groups <- sort(unique(unlist(strsplit(rawNames, ',', fixed = TRUE))))
  }
  if (multiple) {
    groupsids <- NULL
    if (! is.null(groups)) {
      groupsids <- seq(length(originalList) + 1 + 1 * addAllPattern,
                       length(originalList) + length(groups) + 1 * addAllPattern)
      theList <- c(paste0(str_pad(paste(theList), max(nchar(originalList), 10), side = "right"), " ", names(theList)),
                   paste("Group:", groups))
      m <- c(m, paste0(str_pad("", max(nchar(originalList), 10) + nchar(length(theList) + 2) + 2, side = "right"),
                       " Group\n"))
    }
    # add all and regex pattern as options
    if (addAllPattern) {
      theList <- c("all", theList, "Search pattern by regular expression...", "Search by fixed pattern...")
      a <- 1
      p <- length(theList) - 1
      f <- length(theList)
    }
  }
  m <- c(m, paste(paste0(str_pad(seq_along(theList), nchar(length(theList)), side = "left"),
                        if (addAllPattern) c(",a", rep("", length(theList) - 3), ",p", ",f"), ": ", theList),
                  collapse = "\n"))
  m <- c(m, "\n", errormessage, userinfo,
            paste0("\nNumber", if (multiple) "s entered as 2,4:6,9", " or leave empty:"))
  if (isFALSE(userinput)) { # print options and ask for userinput
    message(m)
    userinput <- getLine()
  }
  # interpret userinput and perform basic checks
  identifier <- try(eval(parse(text = paste("c(", gsub("-", ":", userinput), ")"))), silent = TRUE)
  if (! all(grepl(if (addAllPattern) "^[afp0-9,: -]*$" else "^[0-9,: -]*$", userinput)) || inherits(identifier, "try-error")) {
    err <- paste0("Try again, you have to choose some numbers. ", attr(identifier, "condition"), "\n")
    return(chooseFromList(originalList, type, userinfo, addAllPattern, returnBoolean, multiple, errormessage = err))
  }
  # check whether all input is usable
  if (! multiple && length(identifier) > 1) {
    err <- paste0("Try again, multiple chosen: ", substr(paste(identifier, collapse = ", "), 1, 240), "...\n")
    return(chooseFromList(originalList, type, userinfo, addAllPattern, returnBoolean, multiple, errormessage = err))
  }
  if (any(! identifier %in% seq_along(theList))) {
    err <- paste0("Try again, not all in list: ", substr(paste(identifier, collapse = ", "), 1, 240), "...\n")
    return(chooseFromList(originalList, type, userinfo, addAllPattern, returnBoolean, multiple, errormessage = err))
  }
  if (multiple) {
    all <- addAllPattern && any(identifier == "1")
    pattern <- addAllPattern && any(identifier == length(theList) - 1)
    fixed <- addAllPattern && any(identifier == length(theList))
    if (all) { # all
      identifier <- seq_along(originalList)
    } else {
      # interpret group inputs and select all group members
      selectedGroups <- sub("^Group: ", "", theList[intersect(identifier, groupsids)])
      for (group in selectedGroups) {
         identifier <- c(identifier,
                         which(grepl(paste0("(^|,)", group, "($|,)"), names(originalList), perl = TRUE))
                         + addAllPattern)  # shift all identifiers by one if the "all" pattern is the first option
      }
      identifier <- unique(c(identifier[! identifier %in% groupsids]))
      # if search by pattern is selected, ask for pattern and interpret it
      if (pattern) {
        patternid <- choosePatternFromList(originalList, type, fixed = FALSE)
        identifier <- unique(c(identifier, patternid + 1))
      }
      if (fixed) {
        patternid <- choosePatternFromList(originalList, type, fixed = TRUE)
        identifier <- unique(c(identifier, patternid + 1))
      }
      identifier <- identifier[identifier < length(originalList) + 2] - 1 * addAllPattern # if addAllPattern = TRUE, '1' is 'all' option
    }
  }
  booleanList[identifier] <- TRUE
  msgselected <- originalList[identifier]
  stopafter <- min(which(cumsum(nchar(msgselected)) > getOption("chooseFromListLimit", 666)), length(msgselected))
  message("Selected: ", paste(msgselected[seq_len(stopafter)], collapse = ", "), if (stopafter < length(msgselected)) ", ...")
  if (returnBoolean) return(booleanList) else return(originalList[identifier])
}

choosePatternFromList <- function(theList, type = "items", pattern = FALSE, fixed = FALSE) {
  confirmchoice <- FALSE
  if (isFALSE(pattern)) {
    confirmchoice <- TRUE
    message("\nInsert the ", if (fixed) "search pattern with fixed=TRUE: " else "regular expression: ")
    pattern <- getLine()
  }
  id <- grep(pattern = pattern, theList, fixed = fixed)
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
      return(choosePatternFromList(theList, type, fixed = fixed))
    }
  }
  return(id)
}
