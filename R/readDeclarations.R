#' readDeclarations
#'
#' Reads all declarations given in a GAMS code and returns them.
#'
#'
#' @param file A gams file or a vector containing GAMS code.
#' @param unlist A logical indicating whether the output should be returned as
#' a list separated by object type or a matrix.
#' @param types of declarations to be read.
#' @return Either a list of declared objects or a matrix containing object
#' name, the sets the object depends on and the description.
#' @author Jan Philipp Dietrich
#' @importFrom stringr str_sub str_extract str_replace_all str_replace fixed
#' @export
#' @seealso \code{\link{codeCheck}}
readDeclarations <- function(file,
                             unlist = TRUE,
                             types = c("scalar",
                                       "(positive |)variable",
                                       "parameter",
                                       "table",
                                       "equation",
                                       "set")) {
  d <- GAMScodeFilter(readFileOrVector(file))
  endings <- grep(";[   ]*(!!|$)", d)
  out <- list()
  for (t in types) {
    tname <- sub("\\([^\\)]*\\)", "", t)
    if (tname == "table") {
      tname <- "parameter"
    }
    startings <-
      grep(paste0("^[ \\t]*", t, "[s]?([ \\t]|$)"), d, ignore.case = TRUE)
    for (s in startings) {
      if (any(endings >= s)) {
        e <- min(endings[endings >= s])
      } else {
        e <- length(d)
      }
      # cut all object declarations of the given type
      tmp <- d[s:e]
      # extract quoted descriptions
      quotedDescription <- str_sub(str_extract(tmp, "\"[^\"]*\""), 2, -2)
      names(quotedDescription) <- paste0("[#DESC#", seq_along(tmp), "#]")
      # insert placeholders
      tmp <- str_replace(tmp, "\"[^\"]*\"", names(quotedDescription))
      # remove ;
      tmp <- sub(";", "", tmp)
      # remove $-expressions
      tmp <- sub("^(\\$|[[:space:]]*\\$\\$).*", "", tmp)
      # remove type name
      tmp <- sub(paste0("^[ \\t]*", t, "[^ \\t]*"), "", tmp, ignore.case = TRUE)
      # remove "/ xyz /" entries
      tmp <- sub("/.*/", "", tmp)
      .rmFilling <- function(x) {
        n <- grep("/", x)
        if (length(n) < 2)
          return(x)
        rm <- NULL
        for (i in seq(1, length(n) - 1, 2)) {
          rm <- c(rm, (n[i] + 1):n[i + 1])
          x[n[i]] <- sub("/.*$", "", x[n[i]])
        }
        keep <- setdiff(seq_along(x), rm)
        return(x[keep])
      }
      tmp <- .rmFilling(tmp)
      tmp <- gsub("\t", " ", tmp)
      # remove all lines with no objects in them
      tmp <- grep("^ *.{0,1} *$", tmp, invert = TRUE, value = TRUE)
      structure <- "^ *([^ ,\\(]+)(\\([^\\)]+\\)|) *(.*)$"
      # store name
      names <- sub(structure, "\\1", tmp)
      # store sets
      sets <- sub(structure, "\\2", tmp)
      sets <- gsub("\\(", "", sets)
      sets <- gsub("\\)", "", sets)
      sets <- gsub(" +", "", sets)
      # store description (remove "," and empty entries)
      description <- sub("^ *$", "", gsub(",", "", sub(structure, "\\3", tmp)))
      # remove values at the end, if given
      description <- sub(" */.*$", "", description)
      description <- str_replace_all(description, fixed(quotedDescription))
      tmp <- cbind(names, sets, description)
      out[[tname]] <- rbind(out[[tname]], tmp)
    }
  }
  if (unlist) {
    tmp <- NULL
    for (n in names(out)) {
      tmp <- rbind(tmp, cbind(out[[n]], type = n))
    }
    out <- tmp
  }
  return(out)
}
