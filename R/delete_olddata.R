#' delete_olddata
#' 
#' Delete data provided in mapping
#'  
#' @param x Filepath or data frame containing the mapping of files to be deleted
#' @export
#' @author Jan Philipp Dietrich, David Klein

delete_olddata <- function(x) {
  if(is.character(x)) {
    if(!file.exists(x)) stop("Cannot find file mapping!")
    map <- read.csv(x, sep = ";", stringsAsFactors = FALSE, comment.char = "#")
  } else {
    map <- x
  }
  x <- map$file
  names(x) <- map$destination
  for(i in 1:length(x)) {
    outputpath <- Sys.glob(path(names(x)[i],x[i]))
    for(file in outputpath) file.remove(file)
  }
}
