#' Library cleanup tool
#' 
#' Cleans up R library directory. Three steps are taken at the moment:
#' .Rcheck-folders are deleted, old packages are removed, missing zip or tar.gz
#' packages are created (for zip files a Windows system is required).
#' 
#' 
#' @usage maintain_libraries(libpath=".")
#' @param libpath Parent path of the library folder
#' @author Jan Philipp Dietrich
#' @export
#' @seealso \code{\link{buildLibrary}}
#' @examples
#' 
#' \dontrun{maintain_libraries()}
#' 
maintain_libraries <- function(libpath=".") {
  dd <- getwd()
  setwd(libpath)
  
  system("svn update")
  
  #Remove all existing Rcheck-folders
  rcheckfolders <- grep(".Rcheck$",base::list.dirs(full.names = FALSE,recursive = FALSE),value=TRUE)
  unlink(rcheckfolders,recursive=TRUE)
  
  #Remove older packages
  tmp <- system("svn status -vN",intern=TRUE)
  #removed unmanaged or deleted files from list
  tmp <- grep("^(\\?|D)",tmp,value=TRUE,invert=TRUE)
  #remove unneeded information
  tmp <- sub("^.* ([^ ]*)$","\\1",tmp)
  managed_libraries <- base::list.dirs(full.names = FALSE,recursive = FALSE)[base::list.dirs(full.names = FALSE,recursive = FALSE) %in% tmp]
  for(m in managed_libraries) {
    cat("check",m,"...\n")
    packages <- gtools::mixedsort(grep(paste(m,"_",sep=""),tmp,value=TRUE))
    topversion <- NULL
    for(ty in c("zip$","tar.gz$","tgz$")) {
      tmp2 <- grep(ty,packages,value=TRUE)
      if(length(tmp2)>1) system(paste("svn remove",paste(tmp2[1:(length(tmp2)-1)],collapse=" "))) 
      topfile <- ifelse(length(tmp2)>0,tmp2[length(tmp2)],"dummy_0.0.zip")
      if(ty!="tgz$") topversion <- c(topversion,topfile) #exclude mac version
    }
    toptmp <- topversion
    topversion <- as.numeric(sub("^.*_(.*)\\.(zip|tar\\.gz)$","\\1",topversion)) 
    if(topversion[1]>topversion[2]) { #zip newer than tar.gz
      a <- system(paste("R CMD build",m),intern=TRUE)
      ff <- sub("^.*( |')([^ ']*\\.(zip|tar\\.gz)).*$","\\2",grep("\\.(zip|tar\\.gz)",a,value=TRUE))
      if(toptmp[2]!="dummy_0.0.zip") system(paste("svn remove",toptmp[2]))
      system(paste("svn add",ff))
      cat(ff," created and added!\n")
    }
    OS<-Sys.info()["sysname"]
    if(OS=="Windows") {
      if(topversion[1]<topversion[2]) { #zip newer than tar.gz
        a <- system(paste("R CMD INSTALL --build",m),intern=TRUE)
        ff <- sub("^.*( |')([^ ']*\\.(zip|tar\\.gz)).*$","\\2",grep("\\.(zip|tar\\.gz)",a,value=TRUE))
        if(toptmp[1]!="dummy_0.0.zip") system(paste("svn remove",toptmp[1]))
        system(paste("svn add",ff))
        cat(ff," created and added!\n")
      }
    }
  } 
  setwd(dd)
  cat("done!\n")
}