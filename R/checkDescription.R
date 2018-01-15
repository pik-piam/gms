#' checkDescription
#' 
#' Checks whether all Declarations of a GAMS code come with a Description,
#' throws out a warning in case of a missing description.
#' 
#' 
#' @usage checkDescription(x)
#' @param x GAMS declarations matrix as returned by
#' \code{\link{readDeclarations}}
#' @return Nothing is returned.
#' @author Jan Philipp Dietrich
#' @export
#' @seealso \code{\link{codeCheck}}
checkDescription <- function(x) {
  if(is.list(x)) x <- x$declarations
  miss <- which(x[,"description"]=="")
  for(i in miss) warning(dimnames(x)[[1]][i],": missing description for ",x[i,"names"],call.=FALSE)
}
