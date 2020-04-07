#' settingsCheck
#' 
#' Checks GAMS setglobals in code for consistency. Creates a warning if a
#' setglobal command for an existing module is missing or a module is set to a
#' realization which does not exist.
#' 
#' 
#' @param path path of the main folder of the model
#' @param modulepath path to the module folder relative to "path"
#' @return Nothing is returned.
#' @author Jan Philipp Dietrich
#' @export
#' @seealso \code{\link{codeCheck}}
settingsCheck <- function(path=".",modulepath="modules") {
  s <- readSetglobals(path(path,"main.gms"))
  m <- getModules(modulepath)
  dimnames(m)[[1]] <- m[,"name"]
  not_set <- m[!(m[,"name"] %in% names(s)),"name"]
  if(length(not_set)>0) {
    for(n in not_set) warning("setglobal for module ",n," could not be found! This module is not executed at all!",call.=FALSE)
  }                                              
  modules <- names(s)[(names(s) %in% m[,"name"])]
  for(x in modules) {
    realizations <- strsplit(m[x,"realizations"],",")[[1]]  
    if(!(s[x] %in% realizations)) warning("Non-existent realization \"",s[x],"\" set for module \"",x,"\"!",call.=FALSE) 
  }
  
}