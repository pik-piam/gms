#' System Command Available
#' 
#' Checks whether a system command is available (does not return 
#' an error), or not
#' 
#' @param command System command to be tested
#' @return Boolean indicating whether the command is available, or not
#' @author Jan Philipp Dietrich
#' @export
#' @examples
#' SystemCommandAvailable("ls") 
#' 
SystemCommandAvailable <- function(command) {
  out <- suppressWarnings(ifelse(system2(command,stdout=FALSE,stderr=FALSE) != 127, TRUE, FALSE))
  return(out)
}

