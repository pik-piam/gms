#' getLine
#'
#' Get one line of user input regardless of whether running interactively or not (via Rscript).
#' (base::readline does not wait for user input when running via Rscript.)
#'
#' @return The user input as a string.
#'
#' @importFrom withr local_connection
#' @export
getLine <- function() {
  if (interactive()) {
    # needed for e.g. RStudio and R in jupyter
    return(readline())
  }
  return(readLines(withr::local_connection(file("stdin")), n = 1))
}
