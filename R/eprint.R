#' extended Print
#' 
#' An extended print command which formats information in a way that it is good
#' to use for a log-file
#' 
#' 
#' @param var_name name of the variable that should be printed as string
#' @param envir environment from which the variable should be read (by default
#' the environment from which the function is called)
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{eprint_list}}
#' @examples
#' 
#' \dontrun{a <- 1:3
#' eprint("a")}
#' 
###print additional information concerning loaded configuration###
###ePrint (extended Print) offers an extended output functionality which
###allows to create easily log-files with all relevant information

eprint <- function(var_name, envir=parent.frame()) {
  print(paste(var_name,"<-",try(get(var_name,envir=envir),silent=TRUE)),quote=FALSE)
}