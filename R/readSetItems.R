#' readSetItems
#'
#' Parses the sets for a given code file and stores them in a named list.
#'
#' @param codeFile a GAMS code file
#' @param name A name indicating what collection of code files this is (e.g.
#' module name)
#' @param warn A boolean indicating if warnings should be displayed when parsing
#' sections fails
#' @author Falk Benke
readSetItems <- function(codeFile, name = NULL, warn = FALSE) {
  d <- GAMScodeFilter(readFileOrVector(codeFile))

  endings <- grep(";[   ]*(!!|$)", d)

  startings <-
    grep("^[ \\t]*set[s]?([ \\t]|$)", d, ignore.case = TRUE)

  setList <- NULL

  # iterate over all sets sections in the file
  for (s in startings) {
    if (any(endings >= s)) {
      e <- min(endings[endings >= s])
    } else {
      e <- length(d)
    }

    # cut all set declarations
    tmp <- d[s:e]
    # remove quoted descriptions
    tmp <- str_replace(tmp, "\"[^\"]*\"", "")
    # remove ;
    tmp <- sub(";", "", tmp)
    # remove set keyword
    tmp <- sub("set[s]?|SET[S]?", "", tmp)
    # remove if clauses
    tmp[grepl("\\$ifthen|\\$endif", tmp)] <- ""
    # remove blanks
    tmp <- trimws(tmp)
    # remove empty lines or "sets" lines
    tmp <- tmp[!grepl("^$", tmp)]

    # extract sets section with complete definition in one line ----
    if (length(tmp) == 1) {
      title <- gsub("^([a-zA-Z0-9_]+) */(.*)/$", "\\1", tmp)
      items <- gsub("^([a-zA-Z0-9_]+) */(.*)/$", "\\2", tmp)
      items <- strsplit(gsub(" *", "", items), ",")
      names(items) <- title
      setList <- c(setList, items)
      next()
    }

    # extract sets with entire definition in one line ----
    items <- tmp[grep("^.+/.*/$", tmp)]
    if (length(items) > 0) {
      nm <- strsplit(trimws(gsub("^(.+)/(.*)/$*", "\\1", items)), ",")
      items <- trimws(gsub("^(.+)/(.*)/$*", "\\2", items))
      names(items) <- nm
      setList <- c(setList, items)
      # drop parsed sets
      tmp <- tmp[-c(grep("^.+/.*/$", tmp))]
    }

    # extract sets with all items in one line ----
    items <- tmp[grep("^/.*/$", tmp)]
    if (length(items) > 0) {
      items <- strsplit(gsub("/ *", "", items), ",")
      names(items) <- tmp[grep("^/.*/$", tmp) - 1]
      setList <- c(setList, items)
      # drop parsed sets
      tmp <- tmp[-c(grep("^/.*/$", tmp), grep("^/.*/$", tmp) - 1)]
    }

    # identify lines with set name and opening dash OR items and closing dash ----
    if (length(grep("^.+/$", tmp)) > 0) {
      for (i in grep("^.+/$", tmp)) {
        # move opening dash to a separate line
        tmp <- append(tmp, "/", after = i)
      }
      tmp[grep("^.+/$", tmp)] <- trimws(gsub("/", "", tmp[grep("^.+/$", tmp)]))
    }

    # identify lines with set name, opening dash and first items ----
    if (length(grep("^.+/.+$", tmp)) > 0) {
      for (i in grep("^.+/.+$", tmp)) {
        # move opening dash and items to separate lines
        tmp <- append(tmp, "/", after = i)
        tmp <- append(tmp, unlist(strsplit(tmp[grep("^.+/.+$", tmp)], "/"))[2], after = i + 1)
      }
      tmp[grep("^.+/.+$", tmp)] <- trimws(gsub("/.+$", "", tmp[grep("^.+/.+$", tmp)]))
    }

    # no more lines to parse left
    if (length(tmp) == 0) next()

    # extract multiline sets ----
    markers <- grep("^/", tmp)

    if (length(markers) == 0 || length(markers) %% 2 != 0) {
      if (isTRUE(warn)) {
        warning(name, ": Failed to parse section: '", tmp, "'")
      }
      next()
    }

    openingIndices <- markers[seq(1, length(markers), 2)]
    closingIndices <- markers[seq(2, length(markers), 2)]


    # parse all items from a set
    for (i in seq(1, length(openingIndices), 1)) {
      title <- tmp[openingIndices[i] - 1]

      e <- tmp[seq(openingIndices[i], closingIndices[i])]

      # remove beginning and end marker
      e <- gsub("/", "", e)
      # remove opening and closing brackets
      e <- gsub("\\(|\\)| ", "", e)
      # replace dots by comma
      e <- gsub("\\.", ",", e)
      # drop empty lines
      e <- e[!grepl("^$", e)]
      # drop flags, e.g. "%macro%
      e <- trimws(gsub("*%.*%*", "", e))
      # extract all items
      items <- list(unique(unlist(strsplit(e, ","))))
      # assign set name
      names(items) <- title

      setList <- c(setList, items)
    }
  }
  return(setList)
}
