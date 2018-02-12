#' Update package repository
#' 
#' Function to update an package repository. Run this function on a folder which contains
#' packages sources as subfolders. Packages should be either managed via svn or git
#' in order to be updated properly.
#' To add a new package to the repo just checkout/clone it into this folder. The
#' function will automatically detect the new package and add it.
#' 
#' 
#' @param path Path to the repository
#' @param check Boolean deciding whether package must have been checked or not 
#' in order to be distributed
#' @param force_rebuild Option to rebuild all packages from source
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{buildLibrary}}
#' @export

updateRepo <- function(path=".", check=TRUE, force_rebuild=FALSE) {
  cwd <- getwd()
  on.exit(setwd(cwd))
  setwd(path)
  
  if(dir.exists(".svn")) system("svn update")
  
  dirs <- grep("^\\.",list.dirs(recursive = FALSE,full.names = FALSE), value=TRUE, invert=TRUE)
  for(d in dirs) {
    setwd(d)
    if(dir.exists(".svn")) system("svn update")
    if(dir.exists(".git")) system("git pull")
    vkey <- validkey()
    if(!file.exists(paste0("../",d,"_",vkey$version,".tar.gz")) | force_rebuild) {
      if(vkey$valid | !check | force_rebuild) {
        if(vkey$roxygen) suppressWarnings(devtools::document(pkg=".",roclets=c('rd', 'collate', 'namespace', 'vignette')))
        error <- try(devtools::build())
        if("try-error" %in% class(error)) {
          message(".:: ",d," build failed ::.")
        } else {
          message(".:: ",d," build success ::.")
        }
      } else message(".:: ",d," invalid commit ::.")
    }
    setwd("..")
  }
  tools::write_PACKAGES()    
}