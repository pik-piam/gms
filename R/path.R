#' path
#' 
#' Small function to build a consistent path-string based on folder, filename
#' and filetype. The function makes sure that slashes and the dot for the file
#' ending are set correctly (you can supply your folder name either with or
#' without a tailing slash in it. It does not matter.
#' 
#' 
#' @aliases path path
#' @param ... the folders and the file name that should be pasted to a
#' file/folder path
#' @param ftype file type
#' @return A string containing the path combined of folder, filename and
#' filetype
#' @author Jan Philipp Dietrich
path <- function(...,ftype=NULL) {
  if(!is.null(ftype)) if(substring(ftype,1,1)!='.') ftype <- paste('.',ftype,sep='')
  out <- paste(...,sep="/")
  out <- paste(gsub("//","/",out),ftype,sep="")
  first <- list(...)[[1]]
  .tmp <- function(first,out) {
    manipulate <- FALSE
    if(is.null(first)) manipulate <- TRUE
    else if(first=="") manipulate <- TRUE
    if(manipulate) out <- gsub("^/+","",out)
    return(out)
  }
  if(length(first)>1) {
    for(i in 1:length(first)) out[i]<- .tmp(first[i],out[i])
  } else {
    out <- .tmp(first,out)
  }
  return(out)
}
