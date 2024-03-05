#' checkAppearance
#'
#' Checks for all declared objects in which parts of the model they appear and
#' calculates the type of each object (core object, interface object, module
#' object of module xy,...)
#'
#'
#' @param x A code list as returned by \code{\link{codeExtract}}
#' @param capitalExclusionList A vector of names that should be ignored when
#' checking for unified capitalization of variables
#' @return A list with four elements: appearance, setappearance, type and
#' warnings. Appearance is a matrix containing values which indicate whether an
#' object appears in a part of the code or not (e.g. indicates whether "vm_example"
#' appears in realization "on" of module "test" or not.). 0 means that it does not appear,
#' 1 means that it appears in the code and 2 means that it appears in the
#' not_used.txt. setappearance contains the same information but for sets instead of other
#' objects. Type is a vector containing the type of each object (exluding sets). And warnings
#' contains a list of warnings created during that process.
#' @author Jan Philipp Dietrich

#' @export
#' @seealso \code{\link{codeCheck}},\code{\link{readDeclarations}}
checkAppearance <- function(x, capitalExclusionList = NULL) {
  w <- NULL
  ptm <- proc.time()["elapsed"]
  message("  Running checkAppearance...")
  colnames <- unique(names(x$code))
  rownames <- unique(x$declarations[, "names"])

  if (!is.null(x$not_used)) rownames <- unique(c(rownames, x$not_used[, "name"]))

  # check for variables with different capitalization in declarations
  if (length(rownames[duplicated(tolower(rownames))]) > 0) {
    w <- .warning(paste0(
      "Found variables with more than one capitalization in declarations and not_used.txt files: ",
      paste0(rownames[duplicated(tolower(rownames))], collapse = ", ")
    ), w = w)
  }

  # remove right-hand sides in execute_load statements as there are non-module-related
  # object names allowed (here one refers to the names in the gdx file, but these
  # could come from other modules)
  tmp <- grep("execute_load", x$code, ignore.case = TRUE)
  x$code[tmp] <- gsub("=[^,]*", "", x$code[tmp])

  tmp <- sapply(colnames, function(name, x) {
    return(paste(x[names(x) == name], collapse = " "))
  }, x$code)

  # add empty entry in tmp for module realization which do not contain any code but have a not_used.txt
  notUsedNames <- unique(dimnames(x$not_used)[[1]])
  missing <- notUsedNames[!(notUsedNames %in% colnames)]
  if (length(missing) > 0) {
    mtmp <- rep("", length(missing))
    names(mtmp) <- missing
    tmp <- c(tmp, mtmp)
    colnames <- c(colnames, missing)
  }

  declarationsRegex <- paste("(^|[^[:alnum:]_])", escapeRegex(rownames), "($|[^[:alnum:]_])", sep = "")

  message("  Start variable matching...            (time elapsed: ",
          format(proc.time()["elapsed"] - ptm, width = 6, nsmall = 2, digits = 2), ")")

  # This part is the most time consuming (90% of the time in codeCheck). Here, the variable names are searched for in
  # all module realizations. This process primarily seems to scale with the number of variables and not with the number
  # of module realizations. It is hard to optimize since the number of variables that the code has to look for can
  # hardly be reduced
  a <- t(sapply(declarationsRegex, grepl, tmp, perl = TRUE))

  message("  Finished variable matching...         (time elapsed: ",
          format(proc.time()["elapsed"] - ptm, width = 6, nsmall = 2, digits = 2), ")")

  dimnames(a)[[1]] <- rownames
  dimnames(a)[[2]] <- colnames

  # Find variables with different capitalization

  code <- x$code
  # exclude \" comments \"
  code <- gsub("\\\".*\\\"", "", code)
  # exclude any text surrounded by with single quotes
  code <- gsub("'.*'", "", code)
  # exclude display statements
  code <- gsub("display.*", "", code)

  message("  Start var capitalization check...     (time elapsed: ",
          format(proc.time()["elapsed"] - ptm, width = 6, nsmall = 2, digits = 2), ")")

  # check for each variable if it appears with different capitalization in the code
  duplicates <- sapply(declarationsRegex, function(x) {
    # get all lines of code that match the variable (case insensitive)
    chunks <- code[grepl(x, code, ignore.case = TRUE)]
    # if case sensitive search yields less results, there must be occurrences different capitalization
    return(length(chunks) != length(chunks[grepl(x, chunks, ignore.case = FALSE)]))
  })

  if (length(rownames[setdiff(rownames[duplicates], capitalExclusionList)] > 0)) {

    duplicateNames <- unname(setdiff(rownames[duplicates], capitalExclusionList))

    msg <- paste0(
      "Found variables with more than one capitalization in the codebase: ",
      paste0(duplicateNames, collapse = ", "), "\n"
    )

    for (dup in duplicateNames) {
      msg <- paste0(msg, "- Lines found for item '", dup, "':\n")
      dup <- paste("(^|[^[:alnum:]_])", escapeRegex(dup), "($|[^[:alnum:]_])", sep = "")
      chunks <- code[grepl(dup, code, ignore.case = TRUE)]
      msg <- paste0(msg, paste0(setdiff(chunks, chunks[grepl(dup, chunks, ignore.case = FALSE)]), collapse = "\n"))
      msg <- paste0(msg, "\n")
    }

    w <- .warning(msg, w = w)

  }

  message("  Finished var capitalization check...  (time elapsed: ",
          format(proc.time()["elapsed"] - ptm, width = 6, nsmall = 2, digits = 2), ")")

  if (!is.null(x$not_used)) {
    for (i in 1:dim(x$not_used)[1]) {
      if (a[x$not_used[i, "name"], dimnames(x$not_used)[[1]][i]]) {
        w <- .warning(x$not_used[i, "name"], " appears in not_used.txt of module ", dimnames(x$not_used)[[1]][i],
                      " but is used in the GAMS code of it!", w = w)
      }
      a[x$not_used[i, "name"], dimnames(x$not_used)[[1]][i]] <- 2
    }
  }

  sets <- x$declarations[x$declarations[, "type"] == "set", "names"]
  aSets <- a[sets, , drop = FALSE]
  a <- a[!(rownames(a) %in% sets), , drop = FALSE]
  type <- sub("^(o|)[^_]*?(m|[0-9]{2}|)_.*$", "\\1\\2", dimnames(a)[[1]])
  names(type) <- dimnames(a)[[1]]
  return(list(appearance = a, setappearance = aSets, type = type, warnings = w))
}
