#' readSetglobals
#' 
#' Reads all setglobals given in a GAMS code and returns them.
#' 
#' 
#' @param file A gams file or a vector containing GAMS code.
#' @return A vector of values the setglobal variables are set to with setglobal
#' variables as names.
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{readDeclarations}}
readSetglobals <- function(file) {
  f <- readLines(file,warn=FALSE)
  f <- suppressWarnings(grep("^\\$[sS][eE][tT][gG][lL][oO][bB][aA][lL]",f,value=TRUE))
  pattern <- "^\\$[sS][eE][tT][gG][lL][oO][bB][aA][lL] *([^ ]*) *([^ ]*)( .*|)$"
  out <- gsub(pattern,"\\2",f)
  names(out) <- gsub(pattern,"\\1",f)
  return(out)
}