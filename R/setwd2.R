#' setwd2
#' 
#' Mini function that copies in a path from the Clipboard and sets the directory to it.
#' Very useful for Windows users, as it replaces backslashs by slashs.
#' 
#' @usage setwd2(return_only=FALSE)
#' @param return_only if TRUE, the path is not changed, but the clipboard path is returned as string.
#' @return if return_only=FALSE: Nothing, but the working directory is set to. Otherwhise: no working directory returned, but path transformed.
#' @author Benjamin Leon Bodirsky
#' @importFrom utils readClipboard
#' @export

setwd2<-function(return_only=FALSE){
  x=readClipboard()
  x = gsub("\\\\", "/", x)
  if (return_only==FALSE){
    setwd(x) 
  } else {
    return(x)
  }
}
