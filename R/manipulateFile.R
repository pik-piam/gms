#' Replace in File
#' 
#' Function to replace a specific text string in a text file. Useful to
#' manipulate GAMS sourcecode files.
#' 
#' 
#' @usage manipulateFile(file, manipulations,line_endings="win",...)
#' @param file a connection object or a character string describing the file,
#' that should be manipulated.
#' @param manipulations A list of 2 element vectors, containing the search
#' phrase as first element and the replace term as second element. Regular
#' expressions in perl syntax (including backreferencing) can be used.
#' @param line_endings "win" for windows line endings or "os" for line endings
#' in the format of the currently used OS.
#' @param ... Further options passed to gsub
#' @author Jan Philipp Dietrich
#' @export
#' @seealso \code{\link{replace_in_file}},\code{\link{manipulateConfig}}
#' @examples
#' 
#' #manipulateFile("example.txt",list(c("bla","blub"),c("a","b")))
#' 
#' 
manipulateFile <- function(file,manipulations,line_endings="win",...) {
  if(!is.list(manipulations)) manipulations <- list(manipulations)
  f <- readLines(file)
  for(m in manipulations) {
    f <- gsub(m[1],m[2],f,perl=TRUE,...)
  }
  if (line_endings == "win") {
    writeLinesDOS(f,file)      
  } else writeLines(f,file)
}