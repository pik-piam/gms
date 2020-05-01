#' getPackageAuthors
#' 
#' Creates a suggestion for an Authors entry for the DESCRIPTION
#' of a package. Suggestion is based on author information in roxygen
#' headers and authors are ranked based on number of mentionings. 
#' 
#' Please be aware that the output will most likely require some
#' manual processing before it can be used in the DESCRIPTION!
#' 
#' @param folder R folder of the package
#' @author Jan Philipp Dietrich
#' @export
#' 
getPackageAuthors <- function(folder="R") {
  cwd <- getwd()
  on.exit(setwd(cwd))
  setwd(folder)
  
  files <- dir(".")
  authors <- NULL
  for(f in files) {
    pattern <- "#' *@author"
    tmp <- grep(pattern,readLines(f,warn = FALSE),value=TRUE)
    if(length(tmp)>0) authors <- c(authors,strsplit(sub(pattern,"",tmp),",")[[1]])
  }
  authors <- sub("^ *","",authors)
  ranking <- sort(table(authors),decreasing = TRUE)
  
  
  for(i in 1:length(ranking)) {
    name <- sub(" ","\", \"",names(ranking)[i])
    if(i==1) out <- paste0("Authors@R: c(person(\"",name,"\", email = \"@pik-potsdam.de\", role = c(\"aut\",\"cre\")),")
    else if(i==length(ranking)) out <- c(out ,paste0("             person(\"",name,"\", role = \"aut\"))")) 
    else out <- c(out ,paste0("             person(\"",name,"\", role = \"aut\"),"))  
  }
  cat(out,sep = "\n")
}
