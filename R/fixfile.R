#' fixfile
#' 
#' Fix file format of text files (e.g. line/file endings
#' 
#' 
#' @usage fixfile(f)
#' @param f path to the file that should be fixed
#' @author Jan Philipp Dietrich
#' @export
fixfile <- function(f) {
  a <- readLines(f)
  writeLines(a,f)
}