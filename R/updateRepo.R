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
#' @param clean Option to clean repos before updating/pulling to avoid merge conflicts
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{buildLibrary}}
#' @export

updateRepo <- function(path=".", check=TRUE, force_rebuild=FALSE, clean=FALSE) {
  cat(date(),"\n")
  cwd <- getwd()
  on.exit(setwd(cwd))
  setwd(path)
  
  if(dir.exists(".svn")) {
    if(clean) system("svn revert -Rq .")
    system("svn update -q")
  }
  ap <- suppressWarnings(available.packages(paste0("file:",getwd())))
  
  dirs <- grep("^\\.",list.dirs(recursive = FALSE,full.names = FALSE), value=TRUE, invert=TRUE)
  nchar <- max(nchar(dirs))
  for(d in dirs) {
    fd <- format(d,width=nchar)
    curversion <- tryCatch(ap[d,"Version"],error=function(e)return(0))
    setwd(d)
    if(dir.exists(".svn")) {
      if(clean) system("svn revert -Rq .")
      system("svn update -q")
    }
    if(dir.exists(".git")) {
      if(clean) {
        system("git reset --hard HEAD -q")
        system("git clean -fxq")
      }
      system("git pull -q")
    }
    vkey <- validkey()
    if(curversion < vkey$version | force_rebuild) {
      if(vkey$valid | !check | force_rebuild) {
        if(vkey$roxygen) error <- try(devtools::document(pkg=".",roclets=c('rd', 'collate', 'namespace', 'vignette')))
        if(!("try-error" %in% class(error))) error <- try(devtools::build())
        if("try-error" %in% class(error)) {
          message(".:: ",fd," ",curversion," -> ",vkey$version," build failed ::.")
          if(dir.exists(".git")) system("git --no-pager show -s --format='(%h) %s \n%an <%ae>' HEAD")
        } else {
          message(".:: ",fd," ",curversion," -> ",vkey$version," build success ::.")
        }
      } else {
        message(".:: ",fd," ",curversion," -> ",vkey$version," invalid commit ::.")
        if(dir.exists(".git")) system("git --no-pager show -s --format='(%h) %s \n%an <%ae>' HEAD")
      }
    } else message(".:: ",fd," ",format(curversion, width=10)," ok ::.")
    setwd("..")
  }
  tools::write_PACKAGES()    
}
