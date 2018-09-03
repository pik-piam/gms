#' getfiledestinations
#' 
#' Create file2destination mapping based on information from the model
#'  
#' @export
#' @author Jan Philipp Dietrich, David Klein

  
getfiledestinations <- function() {
  folders <- base::list.dirs(recursive=FALSE, full.names=FALSE)
  folders <- grep("^(\\.|225|output|calib_run|figure)",folders, invert=TRUE, value=TRUE)
  files <- NULL
  for(f in folders) files <- c(files,dir(path=f,pattern="^files$",recursive = TRUE, full.names=TRUE))
  out <- NULL
  for(f in files) {
    tmp <- grep("^\\*",readLines(f, warn = FALSE),invert=TRUE,value=TRUE)
    add <- data.frame(file=tmp,destination=dirname(f),stringsAsFactors = FALSE)
    out <- rbind(out,add)
  }
  out <- as.data.frame(lapply(out,trimws),stringsAsFactors=FALSE)
  return(out[out[[1]]!="",])
}
