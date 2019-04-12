#' convert.modularGAMS
#' 
#' Converts modular GAMS code from an older modular definition to the newest one
#' 
#' @param path path to the main folder of the model
#' @param modulepath Module path within the model (relative to the model main
#' folder)
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{codeCheck}}
#' @export
convert.modularGAMS <- function(path=".", modulepath="modules/") {
  cwd <- getwd()
  setwd(path)
  on.exit(setwd(cwd))
  
  version <- is.modularGAMS(".",version=TRUE, modulepath=modulepath)
  if(version==2) {
    message("Code is already following the newest modular structrue (Version 2)")
    return(NULL)
  }
  if(version!=1) stop("Only conversion from modular version 1 to 2 is currently supported!")
  
  m <- getModules(modulepath)
  setwd(modulepath)
  for(i in rownames(m)) {
    setwd(m[i,"folder"])
    file.rename(paste0(m[i,"folder"],".gms"),"module.gms")
    for(r in strsplit(m[i,"realizations"],",")[[1]]) {
      file.rename(paste0(r,".gms"),paste0(r,"/realization.gms"))
    }
    setwd("..")
  }
}