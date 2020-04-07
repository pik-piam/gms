#' codeExtract
#' 
#' Returns aggregated and cleaned GAMS code together with declaration matrix
#' 
#' 
#' @param codeFiles A vector of file names of GAMS code files.
#' @param name A name indicating what collection of code files this is (e.g.
#' module name)
#' @return A list with two elements: code and declarations. Code contains the
#' cleaned up gams code and declarations contains the declarations matrix as
#' returned by \code{\link{readDeclarations}}
#' @author Jan Philipp Dietrich
#' @export
#' @seealso \code{\link{codeCheck}},\code{\link{readDeclarations}}
codeExtract <- function(codeFiles,name) {
  out <- list()
  for(f in codeFiles) out$code <- c(out$code,readLines(f,warn=FALSE))
  out$declarations <- readDeclarations(out$code)
  out$code <- GAMScodeFilter(out$code)
  names(out$code) <- rep(name,length(out$code))
  if(!is.null(out$declarations)) dimnames(out$declarations)[[1]] <- rep(name,dim(out$declarations)[1])
  return(out)
}