#' checkoutLibraries
#' 
#' Checks out the landuse R libraries.
#' 
#' 
#' @usage
#' checkoutLibraries(dirname="libraries",svn_path="http://subversion/svn/magpie/libraries/")
#' @param dirname Name of the directory to which the liraries are checked out.
#' @param svn_path Path of the svn repository.
#' @return Normal exit returns 0, in case of failure, 1 is returned.
#' @author Markus Bonsch
#' @seealso \code{\link{buildLibrary}},\code{\link{commitLibraries}}
#' @export
#' @examples
#' 
#' \dontrun{checkoutLibraries()}
#' 
checkoutLibraries<-function(dirname="libraries",svn_path="http://subversion/svn/magpie/libraries/"){

  ####################################################################
  #Check if the repository is available under svn_path
  ###################################################################
  svn_paths<-c(svn_path,"http://localhost:8888/svn/magpie/libraries/","http://subversion/svn/magpie/libraries/","not_found")
  for(svn_path in svn_paths){
    if(length(suppressWarnings(system(paste("svn ls",svn_path),intern=T,ignore.stderr=T)))>0) break
  }
  if(svn_path=="not_found") {
    warning("Unable to connect to the repository. Check your svn path and internet connection.")
    return(1)
  }
  
  ###################################################################
  #checkout the library to incorporate the new data
  ###################################################################
  thisdir<-getwd()
  fullpath<-path(thisdir,dirname)
  if(system(paste("svn checkout",svn_path,dirname,"--depth empty"))!=0){
    warning("Problems downloading the library from the repository.")
    system(paste("rm -rf",fullpath))
    return(1)
  }
  setwd(dirname)
  if(system("svn up validation")!=0){
    setwd(thisdir)
    system(paste("rm -rf",fullpath))
    warning("Problems downloading the library from the repository.")
    return(1)
  }
  setwd(thisdir)
  return(0)
}
 