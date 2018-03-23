#' checkDescription
#' 
#' Checks whether all Declarations of a GAMS code come with a Description,
#' throws out a warning in case of a missing description.
#' 
#' @param x GAMS declarations matrix as returned by
#' @param w a vector of warnings the warnings should be added to
#' \code{\link{readDeclarations}}
#' @return vector of warnings
#' @author Jan Philipp Dietrich
#' @export
#' @seealso \code{\link{codeCheck}}
checkDescription <- function(x, w=NULL) {
  if(is.list(x)) x <- x$declarations
  miss <- which(x[,"description"]=="")
  for(i in miss) w <- .warning(dimnames(x)[[1]][i],": missing description for ",x[i,"names"],w=w)
  return(w)
}
