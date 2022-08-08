#' chooseFromList
#'
#' Allows the user to select single or multiple elements from a list.
#' Entries can be selected based on individual choice, groups, regex or all.
#'
#' @param theList list to be selected from
#' @param group list with same dimension as theList with group names to allow to select whole groups, optional
#' @param returnBoolean TRUE: returns list with dimension of theList with FALSE or TRUE
#'                      FALSE: returns selected entries of theList
#' @param multiple TRUE: allows to select multiple entries. FALSE: no
#' @param allowEmpty TRUE: allows you not to select anything (returns NA). FALSE: must select something
#' @param type string to be shown to user to understand what they choose
#' @param userinput string provided by the user. If not supplied, user is asked
#'
#' @return either a boolean list with same length as input or only the selected list items.
#'
#' @author Oliver Richters
#'
#' @importFrom stringr str_pad
#'
#' @export
chooseFromList <- function(theList, type = "runs", returnBoolean = FALSE, multiple = TRUE,
                           allowEmpty = FALSE, group = FALSE, userinput = FALSE) {
  originalList <- theList
  booleanList <- rep(FALSE, length(originalList)) # set to FALSE
  if (! isFALSE(group) && isFALSE(multiple)) {
    message("As multiple is FALSE, the group you specified is ignored.")
    group <- FALSE
  }
  if (! isFALSE(group) && length(group) != length(originalList)) {
    stop("group must have same dimension as theList, or multiple not allowed.")
  }
  m <- paste0("\n\nPlease choose ", type, ":\n\n")
  # in group mode, paste group after each entry and add them to theList as options
  if (! isFALSE(group)) {
    groups <- sort(unique(group))
    groupsids <- seq(length(originalList) + 2, length(originalList) + length(groups) + 1)
    theList <- c(paste0(str_pad(theList, max(nchar(originalList), 6), side = "right"), " ", group),
                 paste("Group:", groups))
    m <- c(m, paste0(str_pad("", max(nchar(originalList), 6) + nchar(length(theList) + 2) + 2, side = "right"),
              " Group"))
  }
  # add all and regex pattern as options
  if (multiple) theList <- c("all", theList, "Search by pattern...")
  m <- c(m, paste(paste(str_pad(seq_along(theList), nchar(length(theList)), side = "left"), theList, sep = ": "),
            collapse = "\n"))
  m <- c(m, paste0("\nNumber", if (multiple) "s entered as 2,4:6,9",
            if (allowEmpty) " or leave empty", " (", type, "): "))
  if (isFALSE(userinput)) { # print options and ask for userinput
    message(m)
    userinput <- getLine()
  }
  # split userinput and perform basic checks
  identifier <- strsplit(userinput, ",")[[1]]
  if (allowEmpty && length(identifier) == 0) return(if (returnBoolean) booleanList else NA)
  if (length(identifier) == 0 || ! all(grepl("^[0-9,:]*$", identifier))) {
    message("Try again, you have to choose some numbers.")
    return(chooseFromList(originalList, type, returnBoolean, multiple, allowEmpty, group))
  }
  # go through list of userinput, and turn 2:5 into 2,3,4,5 etc.
  tmp <- NULL
  for (i in seq_along(identifier)) {
    if (isTRUE(length(strsplit(identifier, ":")[[i]]) > 1)) {
      tmp <- c(tmp, as.numeric(strsplit(identifier, ":")[[i]])[1]:as.numeric(strsplit(identifier, ":")[[i]])[2])
    } else {
      tmp <- c(tmp, as.numeric(identifier[i]))
    }
  }
  identifier <- tmp
  # check whether all input is usable
  if (! multiple && isTRUE(length(identifier) > 1)) {
    message("Try again, not in list or multiple chosen: ", paste(identifier, collapse = ", "))
    return(chooseFromList(originalList, type, returnBoolean, multiple, allowEmpty, group))
  }
  if (any(! identifier %in% seq_along(theList))) {
    message("Try again, not all in list: ", paste(identifier, collapse = ", "))
    return(chooseFromList(originalList, type, returnBoolean, multiple, allowEmpty, group))
  }
  # interpret group inputs
  if (! isFALSE(group)) {
    selectedGroups <- sub("^Group: ", "", theList[intersect(identifier, groupsids)])
    identifier <- unique(c(identifier[! identifier %in% groupsids], which(group %in% selectedGroups) + 1))
  }
  # if search by pattern is selected, ask for pattern and interpret it
  if (multiple && length(identifier) == 1 && identifier == length(theList)) {
    message("\nInsert the search pattern or the regular expression: ")
    pattern <- gms::getLine()
    id <- grep(pattern = pattern, originalList)
    # lists all chosen and ask for the confirmation of the made choice
    message("\n\nYou have chosen the following ", type, ":")
    if (length(id) > 0) message(paste(paste(seq_along(id), originalList[id], sep = ": "), collapse = "\n"))
    message("\nAre you sure these are the right ", type, "? (y/n): ")
    if (gms::getLine() == "y") {
      identifier <- id
      booleanList[id] <- TRUE
    } else {
      return(chooseFromList(originalList, type, returnBoolean, multiple, allowEmpty, group))
    }
  # if all was selected, choose all
  } else if (any(theList[identifier] == "all")) {
    booleanList[] <- TRUE
    identifier <- seq_along(originalList)
  } else {
    if (multiple) identifier <- identifier - 1 # correct for all option
    booleanList[identifier] <- TRUE
  }
  message("Selected: ", paste(originalList[identifier], collapse = ", "))
  if (returnBoolean) return(booleanList) else return(originalList[identifier])
}
