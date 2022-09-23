#' readFileOrVector
#'
#' Input: a file name or a character vector. Output: a character vector of the contents of the file, or
#' the character vector unchanged.
#'
#'
#' @param file A file name or a character vector.
#' @return A character vector of the inputs of the file.
#' @author Mika Pflüger
readFileOrVector <- function(file) {
  if (length(file) == 1) {
    if (file == "") return(NULL)
    file <- readLines(file, warn = FALSE)
  }
  return(file)
}
