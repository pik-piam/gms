#' Publish data in a repository
#' 
#' Downloads a list of tgz files from a list of repos, merge them and publish it on another server
#' 
#' @param input a vector of files to be downloaded or a cfg list with settings to be used (e.g. containing
#' cfg$input, cfg$repositories). Settings in the config list will be overwritten by other arguments of
#' this function if they are not set to NULL
#' @param name name of the data to be published (will be used in as file name). If a target vector, or targets 
#' separated by "|" are provided the user will be asked interactively where the file should be written to. 
#' By default it will look for target information in the environment variable PUBLISH_DATA_TARGET
#' @param target target the data should be published in (format user@server:/folder/path)
#' @param ... further options provided to \code{\link{download_unpack}}
#' @seealso \code{\link{download_unpack}},\code{\link{tardir}}
#' @author Jan Philipp Dietrich
#' @importFrom utils untar
#' @export

publish_data <- function(input,name, target=Sys.getenv("PUBLISH_DATA_TARGET", unset = "."), ...) {
  if(length(target)==1) target <- strsplit(target,"|",fixed = TRUE)[[1]]
  
  if(length(target)>1) {
    choose_target <- function(target,title="Please choose target") {
      get_line <- function(){
        # gets characters (line) from the terminal or from a connection
        # and returns it
        if(interactive()){
          s <- readline()
        } else {
          con <- file("stdin")
          s <- readLines(con, 1, warn=FALSE)
          on.exit(close(con))
        }
        return(s);
      }
      cat("\n\n",title,":\n\n")
      cat(paste(1:length(target), target, sep=": " ),sep="\n")
      cat("\nNumber: ")
      identifier <- get_line()
      identifier <- as.numeric(strsplit(identifier,",")[[1]])
      if (any(!(identifier %in% 1:length(target)))) stop("This choice (",identifier,") is not possible. Please type in a number between 1 and ",length(target))
      return(target[identifier])
    }
    target <- choose_target(target)
  }
  tarfile <- paste0(name,".tgz")
  dir <- paste0(tempdir(),"/data")
  tmptarfile <- paste0(tempdir(),"/",tarfile)
  dir.create(dir)
  download_unpack(input, targetdir=dir, ...)
  tardir(dir=dir, tarfile=tmptarfile)
  unlink(dir, recursive = TRUE)
  if(grepl(":",target)) {
    system(paste0("sftp ",target," <<< $'put ",tmptarfile,"'"))
    unlink(tmptarfile)
  } else {
    file.rename(tmptarfile,paste0(target,"/",tarfile))
  }
}