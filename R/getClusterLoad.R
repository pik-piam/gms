#' getClusterLoad
#' 
#' Returns information about cluster load in case that
#' the command "sclass" is available. Otherwise, it 
#' returns NULL
#' 
#' @return NULL, if command "sclass" is not available,
#' otherwise returns a named vector with current load on
#' available partitions
#' @author Jan Philipp Dietrich
#' @export
getClusterLoad <- function() {
  if(!SystemCommandAvailable("sclass")) return(NULL)
  a <- system("sclass",intern=TRUE)
  a <- grep("Partition",a,value=TRUE)
  pattern <- "^.*\"(.*)\".*?([0-9.]+)%.*$"
  load <- as.numeric(sub(pattern,"\\2",a))
  names(load) <- sub(pattern,"\\1",a)
  return(load)
}