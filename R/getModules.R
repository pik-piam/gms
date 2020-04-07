#' getModules
#' 
#' Extract module information of a GAMS model.
#' 
#' 
#' @param modulepath The path where the modules are stored.
#' @return A matrix containing the different modules with name, corresponding
#' module number and corresponding realizations
#' @author Jan Philipp Dietrich
#' @export
#' @importFrom utils data
#' @seealso \code{\link{codeCheck}}
getModules <- function(modulepath) {
  if(!dir.exists(modulepath)) stop("Module path ",modulepath," does not exist!")
  lucode <- NULL
  data("lucode", envir=environment(), package="lucode")
  folder <- base::list.dirs(path=modulepath,full.names = FALSE,recursive = FALSE)
  name   <- gsub("[0-9]+\\_","",folder)
  number <- gsub("([0-9]+)\\_.*","\\1",folder) 
  out    <- cbind(name,number,folder)
  realizations <- folder
  for(i in 1:dim(out)[1]) realizations[i] <- paste(setdiff(base::list.dirs(path=path(modulepath,out[i,"folder"]),full.names = FALSE,recursive = FALSE),lucode$reserved_types),collapse=",")
  out <- cbind(out,realizations)
  rownames(out) <- out[,"name"]
  return(out)  
}
