#' Creative tgz archive from directory
#' 
#' Creates a tgz from all files in a directory
#' 
#' @param dir directory from which the tar file should be generated
#' @param tarfile name of the archive the data should be written to (tgz file)
#' @author Jan Philipp Dietrich
#' @export
#' @examples
#' 
#' \dontrun{
#' tardir(".","test.tgz")}
#' 
tardir <- function(dir=".",tarfile="data.tgz") {
  targetdir <- normalizePath(dirname(tarfile))
  cwd <- getwd()
  on.exit(setwd(cwd))
  setwd(dir)
  trash <- system(paste0("tar -czf \"",targetdir,"/",basename(tarfile),"\" *"), intern = TRUE)
}