#' Extract arguments
#' 
#' Extracts the value (right-hand-side) of a string of the structure
#' "name=value" and converts it to an appropriate format
#' 
#' 
#' @usage extract_arguments(input_arg)
#' @param input_arg string of the structure "name=value"
#' @return \item{value }{the value (right-hand-side) of the string converted
#' into an appropriate format}
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{readArgs}}
#' @examples
#' 
#' \dontrun{
#' extract_arguments("bla=1:9")
#' # [1] 1 2 3 4 5 6 7 8 9
#' 
#' extract_arguments("blub=3,5,7")
#' # [1] 3 5 7
#' 
#' extract_arguments("ble=hallo")
#' # [1] "hallo"
#' }
#' 
#This file reads arguments from command line. To use this script you have to include it by typing source("readArgs.R")
#in your script and call readArgs(allowed_args) including all arguments that can be read from command line

###define function for extraction of indicies sets from command line input###
extract_arguments <- function(input_arg) {
  if(length(grep("=",input_arg)>0)) input_arg <- strsplit(input_arg,"=")[[1]][2]
  if(length(grep(",",input_arg)>0)) return(unlist(sapply(strsplit(input_arg,",")[[1]],extract_arguments),use.names=FALSE)) 
   if(length(strsplit(input_arg,":")[[1]]) == 2) {
    if(suppressWarnings(is.na(as.integer(strsplit(input_arg,":")[[1]][1])) | is.na(as.integer(strsplit(input_arg,":")[[1]][2])))) {
      extract_arguments <- input_arg
    } else {
      extract_arguments <- as.integer(strsplit(input_arg,":")[[1]][1]):as.integer(strsplit(input_arg,":")[[1]][2]) 
    }
  } else {
    if(sum(suppressWarnings(is.na(as.numeric(strsplit(input_arg,",")[[1]])))==0)) {
      extract_arguments <- as.numeric(strsplit(input_arg,",")[[1]])
    } else {      
      if(suppressWarnings(is.na(as.logical(input_arg)))) {
        extract_arguments <- strsplit(input_arg,",")[[1]]
        if(extract_arguments=="NULL") extract_arguments <- NULL
      } else {      
        extract_arguments <- as.logical(input_arg)
      }
    }
  }
  return(extract_arguments)
}