#' get_info
#' 
#' Function to extract information from info.txt
#'  
#' @param file path to info.txt (including info.txt)
#' @param grep_expression String before the information that should be extracted
#' @param sep Separator between grep_expression and information
#' @param pattern Optional pattern to be replaced (default pattern = "")
#' @param replacement Optional replacement (default replacement = "")
#' @export
#' @author Jan Philipp Dietrich, David Klein

get_info <- function(file, grep_expression, sep, pattern="", replacement=""){
  if(!file.exists(file)) return(NULL)
  file <- readLines(file, warn=FALSE)
  tmp <- grep(grep_expression, file, value=TRUE) # pick line
  if(identical(tmp,character(0))) return(NA)
  tmp <- strsplit(tmp, sep)
  tmp <- sapply(tmp, "[[", 2)
  tmp <- gsub(pattern, replacement ,tmp)
  if(all(!is.na(as.logical(tmp)))) return(as.vector(sapply(tmp, as.logical)))
  if (all(!(regexpr("[a-zA-Z]",tmp) > 0))){
    tmp <- as.numeric(tmp)
  }
  return(tmp)
}
