#' copy_input
#' 
#' Function to copy input files to their destination folders
#'  
#' @param x Filepath or data frame containing the mapping of files to be deleted
#' @param sourcepath Path to folder containing all input files
#' @param low_res Jans Geheimnis
#' @param move If TRUE files will be moved instead of copied (default=FALSE)
#' @importFrom magclass copy.magpie
#' @export
#' @author Jan Philipp Dietrich, David Klein
#' @examples
#' \dontrun{copy_input(x = file2destination, sourcepath = "input", low_res = low_res, move = TRUE)}

copy_input <- function(x, sourcepath, low_res, move=FALSE) {
  if(is.character(x)) {
    if(!file.exists(x)) stop("Cannot find file mapping!")
    map <- read.csv(x, sep = ";", stringsAsFactors = FALSE)
  } else {
    map <- x
  }
  x <- map$file
  names(x) <- map$destination
  if(move) {
    cat("\nStart moving input files:\n")
  } else {
    cat("\nStart copying input files:\n")
  }
  
  nmax <- max(nchar(x))
  for(i in 1:length(x)) {
    outputpath <- path(names(x)[i],x[i])
    if(file.exists(outputpath)) file.remove(outputpath)
    inputpath <- paste0(sourcepath,"/",x[i])
    if(!file.exists(inputpath)) {
      inputpath <- Sys.glob(sub("^(.*)\\.[^\\.]*$", paste0(sourcepath,"/\\1_",low_res,".*"), x[i]))
      if(length(inputpath)>1) {
        stop("Problem determining the proper input path for file", x[i], "(more than one possible path found)")
      } else if(length(inputpath)==0) {
        warning("File ", x[i]," seems to be missing!")
        cat("   ",format(x[i],width=nmax)," ->  FAILED!\n")
        next
      }
    }
    copy.magpie(inputpath, outputpath, round=8)
    if(move & !(i != length(x) &  (x[i] %in% x[i+1:length(x)]))) {
      file.remove(inputpath)
    }
    cat("   ",format(x[i],width=nmax)," -> ",outputpath, "\n")
  }
  cat("\n")
}
