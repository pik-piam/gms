#' Setup Info
#' 
#' Returns a list with information about the current session (session info,
#' used library paths and installed libraries)
#' 
#' 
#' @return A list with information about the current session and the currently
#' used R setup.
#' @author Jan Philipp Dietrich
#' @export
#' @importFrom utils installed.packages sessionInfo
#' @examples
#' 
#'  setup_info()
#' 


setup_info <- function() {
 out <- list()
 out$sysinfo <- Sys.info()
 out$sessionInfo <- sessionInfo()
 out$libPaths <- .libPaths()
 out$installedpackages <- installed.packages()  
 return(out)
}