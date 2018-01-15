#' commitLibraries
#' 
#' Commits the landuse R libraries.
#' 
#' 
#' @usage
#' commitLibraries(libpath=".",message="",svn_path="http://subversion/svn/magpie/libraries/")
#' @param libpath Name of the directory from which the libraries shall be
#' committed.
#' @param message SVN commit message.
#' @param svn_path Path of the svn repository.
#' @return Normal exit returns 0, in case of failure, 1 is returned.
#' @author Markus Bonsch
#' @seealso \code{\link{buildLibrary}},\code{\link{checkoutLibraries}}
#' @export
#' @examples
#' 
#' \dontrun{commitLibraries(libpath="./libraries",message="Update of the land output function")}
#' 
commitLibraries<-function(libpath=".",message="",svn_path="http://subversion/svn/magpie/libraries/"){
  if(message!=""){
    test<-system(paste('svn commit -m "',message,'"',libpath))
  } else {
    test<-system(paste('svn commit' ,libpath))
  }
  
  if(test!=0){
      warning("Problems committing the updated library.")
      return(1)
  } else{
    cat("\nLibraries successfully committed.\n")
    return(0)
  } 
}