#' getScenNames
#' 
#' Extract module information of a GAMS model.
#' 
#' 
#' @usage getScenNames(dirs)
#' @param dirs vector of paths to the used output folders.
#' @return A vector containing the titles used as scenario names for e.g. plots
#' @author Lavinia Baumstark
#' @export

getScenNames <- function(dirs){
  path <- path(dirs,"config.Rdata")
  Names <- c()
  for (i in path) {
    load(i)
    if(!exists("cfg")) cfg <- list()
    Names[i] <- cfg$title
  }
  return(Names)
}