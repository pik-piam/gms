#' read_yaml_header
#' 
#' Reads header written in yaml format from a file
#' 
#' 
#' @param file path to the file which contains the YAML header
#' @param n Number of lines to be read (header must be part of these lines in order to be read)
#' @return A list containing the read in information
#' @author Jan Philipp Dietrich
#' @importFrom yaml yaml.load
#' @export

read_yaml_header <- function(file, n=20) {
  tmp <- readLines(file, n = 20)
  range <- grep("# ?-{3}",tmp)
  if(length(range)>2) warning("More than two YAML separators detected, only the first two will be used!")
  if(length(range)<2 || range[1]+1==range[2]) return(NULL)
  out <- yaml.load(sub("^# *","",tmp[(range[1]+1):(range[2]-1)]))
  return(out)
}