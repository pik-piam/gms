#' copy_input
#'
#' Function to copy input files to their destination folders
#'
#' @param x Filepath or data frame containing the mapping of files to be deleted
#' @param sourcepath Path to folder containing all input files
#' @param suffix suffix that might be part of input names that should be deleted
#' @param move If TRUE files will be moved instead of copied (default=FALSE)
#' @export
#' @author Jan Philipp Dietrich, David Klein


copy_input <- function(x, sourcepath, suffix = NULL, move = FALSE) { #nolint
  if (!requireNamespace("magclass", quietly = TRUE)) stop("The package magclass is required for copying files!")
  if (is.character(x)) {
    if (!file.exists(x)) stop("Cannot find file mapping!")
    map <- read.csv(x, sep = ";", stringsAsFactors = FALSE)
  } else {
    map <- x
  }
  x <- map$file
  names(x) <- map$destination
  if (move) {
    message("\nStart moving input files:\n")
  } else {
    message("\nStart copying input files:\n")
  }

  nmax <- max(nchar(x))
  for (i in seq_along(x)) {
    outputpath <- path(names(x)[i], x[i])
    if (file.exists(outputpath)) file.remove(outputpath)
    inputpath <- paste0(sourcepath, "/", x[i])
    if (!file.exists(inputpath)) {
      inputpath <- Sys.glob(sub("^(.*)\\.[^\\.]*$", paste0(sourcepath, "/\\1_", suffix, ".*"), x[i]))
      if (length(inputpath) > 1) {
        stop("Problem determining the proper input path for file", x[i], "(more than one possible path found)")
      } else if (length(inputpath) == 0) {
        warning("File ", x[i], " seems to be missing!")
        message("   ", format(x[i], width = nmax), " ->  FAILED!")
        next
      }
    }
    magclass::copy.magpie(inputpath, outputpath)
    if (move & !(i != length(x) &  (x[i] %in% x[i + seq_along(x)]))) {
      file.remove(inputpath)
    }
    message("   ", format(inputpath, width = nmax), " -> ", outputpath)
  }
  message()
}
