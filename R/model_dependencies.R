#' Function to detect R package dependencies
#' 
#' This function analyzes a model folder and all subfolders and searches
#' for library and require statements.
#' 
#' @param mainfolder main folder of the model to be analyzed
#' @return A list of dependencies sorted by appearances
#' @author Jan Philipp Dietrich
#' @export

model_dependencies <- function(mainfolder=".") {
  cwd <- getwd()
  setwd(mainfolder)
  on.exit(setwd(cwd))
  files <- dir(".",recursive = TRUE)
  files <- grep("\\.([rR]|[rR]md)$",files, value=TRUE)
  out <- NULL
  where <- NULL
  for(f in files) {
    tmp <- readLines(f,warn = FALSE)
    tmp <- grep("(library|require|requireNamespace|loadNamespace)\\(",tmp,value=TRUE)
    out <- c(out,tmp)
    where <- c(where,rep(f,length(tmp)))
  }
  out <- gsub("\"","",sub('^.*(library|require|requireNamespace|loadNamespace)\\("?([^\\,]*)"?[\\,\\)].*$',"\\2",out),fixed=TRUE)
  out <- data.frame(package=out,where=where, stringsAsFactors = FALSE)
  un <- unique(out$package)
  l <- list()
  for(u in un) {
    l[[u]] <- out$where[out$package==u]
  }
  return(l[order(sapply(l, length), decreasing=TRUE)])
}