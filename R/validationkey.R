#' validationkey
#' 
#' Support function which creates a key out of a version date combination
#' 
#' This function is used in \code{\link{buildLibrary}} to offer the package
#' publication server an option to check whether the package has been properly
#' and successfully (no errors/warnings/notes) checked before its commit.
#' 
#' The calculated key is not safe and can easily be reproduced, but should be
#' complicated enough to encourage users running the \code{\link{buildLibrary}}
#' check properly.
#' 
#' @param version Version number of the package
#' @param date Date of the package
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{buildLibrary}}

validationkey <- function(version,date) {
  return(as.numeric(gsub(".","",as.character(version),fixed=TRUE))*as.numeric(date))
}