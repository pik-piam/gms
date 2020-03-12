#' update_fulldataOutput
#' 
#' Creates GAMS code which stores automatically the levels and marginals of all
#' equations and variables in time depending parameters.
#'  
#' @param modelpath Path of the Model version that should be updated (main
#' folder).
#' @param modulepath Module path within the model (relative to the model main
#' folder)
#' @param corepath Core path within the model (relative to the model main
#' folder)
#' @param loopset Set over which loop runs
#' @author Jan Philipp Dietrich, Felicitas Beier
#' @export
#' @seealso \code{\link{fulldataOutput}},\code{\link{replace_in_file}}

update_fulldataOutput <- function(modelpath=".",modulepath="modules",corepath="core",loopset="t") {
  sets.gms <- suppressWarnings(readLines(path(modelpath,corepath,"sets.gms")))
  types <- strsplit(grep("^[^a-z]*type",sets.gms,value=TRUE),"/")[[1]][2]
  types <- gsub(" ","",strsplit(types,",")[[1]])

  fulldataOutput(path(modelpath,corepath,"declarations.gms"),path(modelpath,corepath,"calculations.gms"),types=types)
  d1 <- base::list.dirs(path=path(modelpath,modulepath),full.names = FALSE,recursive = FALSE)
  for(i in d1) {
    d2 <- base::list.dirs(path=path(modelpath,modulepath,i),full.names = FALSE,recursive = FALSE)
    for(j in d2) {
      fulldataOutput(path(modelpath,modulepath,i,j,"declarations.gms"),path(modelpath,modulepath,i,j,"postsolve.gms"),types=types,warn=FALSE,loopset=loopset)
    }
  }
}
