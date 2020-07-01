#' Creative tgz archive from directory
#' 
#' Creates a tgz from all files in a directory
#' 
#' @param dir directory from which the tar file should be generated
#' @param tarfile name of the archive the data should be written to (tgz file)
#' @author Jan Philipp Dietrich
#' @export
#' @examples
#' # copy dummymodel to temporary directory and compress it
#' file.copy(system.file("dummymodel",package="gms"),tempdir(), recursive = TRUE)
#' model   <- paste0(tempdir(),"/dummymodel")
#' archive <- paste0(tempdir(),"/dummymodel.tgz")
#' tardir(model,archive)
#' 
tardir <- function(dir=".",tarfile="data.tgz") {
  targetdir <- normalizePath(dirname(tarfile))
  cwd <- getwd()
  on.exit(setwd(cwd))
  setwd(dir)
  trash <- system(paste0("tar -czf \"",targetdir,"/",basename(tarfile),"\" *"), intern = TRUE)
}