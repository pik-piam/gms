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
  cat(date(),"\n")
  cwd <- getwd()
  on.exit(setwd(cwd))
  setwd(path)
  
  if(dir.exists(".svn")) {
    system("svn revert -R .")
    system("svn update")
  }
  ap <- suppressWarnings(available.packages(paste0("file:",getwd())))
  
  dirs <- grep("^\\.",list.dirs(recursive = FALSE,full.names = FALSE), value=TRUE, invert=TRUE)
  for(d in dirs) {
    curversion <- tryCatch(ap[d,"Version"],error=function(e)return(0))
    setwd(d)
    if(dir.exists(".svn")) {
      system("svn revert -R .")
      system("svn update")
    }
    if(dir.exists(".git")) {
      system("git reset --hard HEAD")
      system("git clean -fxq")
      system("git pull")
    }
    vkey <- validkey()
    if(curversion < vkey$version | force_rebuild) {
      if(vkey$valid | !check | force_rebuild) {
        if(vkey$roxygen) suppressWarnings(devtools::document(pkg=".",roclets=c('rd', 'collate', 'namespace', 'vignette')))
        error <- try(devtools::build())
        if("try-error" %in% class(error)) {
          message(".:: ",d," ",curversion," -> ",vkey$version," build failed ::.")
        } else {
          message(".:: ",d," ",curversion," -> ",vkey$version," build success ::.")
        }
      } else message(".:: ",d," ",curversion," -> ",vkey$version," invalid commit ::.")
    } else message(".:: ",d," ",curversion," ok ::.")
    setwd("..")
  }
  tools::write_PACKAGES()    
}
