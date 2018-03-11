#' modules_interfaceplot
#' 
#' Function that applies the \code{\link{interfaceplot}} for a whole model and
#' all its modules.
#' 
#' 
#' @usage modules_interfaceplot(x=".", modulepath="modules", filetype="png")
#' @param x Either the object returned by \code{\link{codeCheck}} or the path
#' to the main folder of the model.
#' @param modulepath Path to the modules folder
#' @param filetype Filetype that should be used (e.g. "png" or "pdf")
#' @return A list with interface tables for each module
#' @author Jan Philipp Dietrich
#' @export
#' @seealso \code{\link{codeCheck}},\code{\link{interfaceplot}}
#' @importFrom utils write.table
modules_interfaceplot <- function(x=".",modulepath="modules", filetype="png") {
  if(is.character(x)) x <- codeCheck(x,modulepath)  
  tmp <- interfaceplot(x,interactive=FALSE,modulepath=modulepath,showInterfaces=FALSE,filename = "interfaces", filetype = filetype)
  out <- list()
  for(d in base::list.dirs(path=modulepath,full.names = FALSE,recursive = FALSE)) {
    tmp <- try(interfaceplot(x,interactive=FALSE,modulepath=modulepath,modules=sub("^[^_]*_","",d),showInterfaces=TRUE, filename=path(modulepath,d,paste("interfaces",sub("^[^_]*_","",d),sep="_")), filetype=filetype))
    if(class(tmp)!="try-error") {
      colnames(tmp) <- c("from","to","no. of interfaces","interfaces")
      out[[d]] <- as.data.frame(tmp,stringsAsFactors = FALSE)
      write.table(tmp,path(modulepath,d,paste("interfaces",sub("^[^_]*_","",d),sep="_"),ftype="csv"),row.names=FALSE,sep=",",quote=FALSE, eol="\r\n")
    }
  }  
  return(out)
}