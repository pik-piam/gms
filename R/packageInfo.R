#' packageInfo
#' 
#' Function to print version number and time since last update 
#' formatted to the standard output
#' 
#' @param package Package name
#' @author Jan Philipp Dietrich
#' @importFrom utils packageVersion
#' @export
#' @examples
#' packageInfo("lucode")
#' 

packageInfo <- function(package) {
  version <- try(packageVersion(package), silent=TRUE)
  if("try-error" %in% class(version)) {
    version <- "<not installed>"
    installed <- "<never>"
  } else {
    version <- as.character(version)
    installed <- paste(as.integer(difftime(Sys.time(),file.mtime(system.file("DESCRIPTION", package=package)), units="mins")), "minutes ago")
  }
  cat("\nPackage:", package,"\n")
  cat("Installed version:", version, "\n")
  cat("Last updated:", installed, "\n\n")
}