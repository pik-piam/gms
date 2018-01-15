#' @title functionHeaderDefaults
#' @description used to quickly read in the default values of a function
#'
#' @param ... parameters that shall be assigned to the global environment of R
#'
#' @return no direct return, values are assigned to .GlobalEnv
#' @author Benjamin Leon Bodirsky
#' 
#' @examples
#' 
#' \dontrun{ 
#' 
#' test<-function(a="klk",b="kjlkv",kk=3) {paste(a,b,kk)}
#' functionHeaderDefaults(a="klk",b="kjlkv",kk=3)
#' print(a)
#' paste(a,b,kk)
#' 
#' }
#' 
#' @export


functionHeaderDefaults<-function(...){
  parameters<-list(...)
  for(i in 1:length(parameters)){
    assign(names(parameters)[i],parameters[[i]])
  }
}