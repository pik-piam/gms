#' GAMScodeFilter
#' 
#' Cleans GAMS code supplied from empty lines and comments.
#' 
#' 
#' @param x A vector with lines of GAMS code (as you get by reading the code
#' with readLines)
#' @return The cleaned GAMS code
#' @author Jan Philipp Dietrich
#' @export
#' @seealso \code{\link{readDeclarations}}
#' @examples
#' 
#' GAMScodeFilter(c("","*comment","a=12;","","b=13;"))
#' 
GAMScodeFilter <- function(x) {
  x <- grep("^\\*",x,value=TRUE,invert=TRUE,useBytes=TRUE) #remove comments
  x <- grep("^$",x,value=TRUE,invert=TRUE,useBytes=TRUE)   #remove empty lines
  x <- sub(" *!!.*$","",x)                                 #remove !! comments
  ontexts  <- grep("^\\$ontext",x,ignore.case=TRUE)
  offtexts <- grep("^\\$offtext",x,ignore.case=TRUE)
  remove_ontext <- ifelse(length(ontexts)>0,TRUE,FALSE)
  if(length(ontexts) != length(offtexts)) {
   warning("Inconsistent code! Different number of $ontext and $offtext! Removal of $ontext and $offtext passages deactivated!")
   remove_ontext <- FALSE
  }
  if(any(ontexts >= offtexts)) {
   warning("$offtext appears before $ontext! Removal of $ontext and $offtext passages deactivated!")
   remove_ontext <- FALSE
  }
  if(remove_ontext) {
    remove <- NULL
    for(i in 1:length(ontexts)) {
      remove <- c(remove,ontexts[i]:offtexts[i])
    }
    x <- x[-remove]
  }
  return(x)
}
