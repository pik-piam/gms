#' is.modularGAMS
#' 
#' Checks whether a folder seems to contain modular GAMS code or not. 
#' 
#' @param path path to the main folder of the model
#' @param version if TRUE returns the version of the modular structure or FALSE, 
#' otherwise returns a boolean indicating whether it is modular or not.
#' @param modulepath Module path within the model (relative to the model main
#' folder)
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{codeCheck}}
#' @export
#' @examples
#' is.modularGAMS(system.file("dummymodel",package="gms"))
#' 
is.modularGAMS <- function(path=".",version=FALSE, modulepath="modules/") {
  cwd <- getwd()
  setwd(path)
  on.exit(setwd(cwd))
  if(!file.exists("main.gms")) return(FALSE)
  if(!dir.exists(modulepath)) return(FALSE)
  if(!version) return(TRUE)
  if(length(Sys.glob(paste0(modulepath,"/*/module.gms")))==0) return(1)
  return(2)
}
