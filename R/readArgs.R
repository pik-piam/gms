#' Read Arguments from command line
#' 
#' Function reads arguments from command line of the structure value=content
#' and transforms them to R-Values, if they are called as allowed arguments.
#' 
#' @param \dots arguments allowed to be read from command line (other values
#' are ignored). Value is set if found on command line input, nothing is done,
#' if value is not found.
#' @param .envir environment in which the variables should be written (by
#' default the environment from which the function is called)
#' @param .silent boolean which allows to suppress status messages
#' @author Jan Philipp Dietrich
#' @export
#' @seealso \code{\link{manipulateConfig}}
#' @examples
#' 
#' # Create an R-file "test.R" with following code:
#' value1 <- "old"
#' value2 <- 2
#' value3 <- "willstaythesame"
#' readArgs("value1","value2")
#' print(value1)
#' print(value2)
#' print(value3)
#' 
#' #Open the command line and execute the following code:
#' #Rscript test.R value1=new value2=3 value3=isnotallowed
#' 
#' #Output:
#' # [1]
#' # [1] ### READ COMMAND LINE - ASSIGNED CONFIGURATION ###
#' # [1] value1 <- new
#' # [1] value2 <- 3
#' # [1] ### READ COMMAND LINE - CONFIGURATION END ###
#' # [1]
#' # [1] "new"	
#' # [1] 3
#' # [1] "willstaythesame"
#' 
#' 
###function that reads all allowed arguments from command line###
readArgs <- function(..., .envir=parent.frame(), .silent=FALSE) {
  allowed_args <- c(...)

  ###apply additional command line arguments###
  if (length(commandArgs()) > 5)
  for(argnr in 6:length(commandArgs())) {
    for(i in 1:length(allowed_args)) {
      if (strsplit(commandArgs()[argnr],"=")[[1]][1]==allowed_args[i]) assign(allowed_args[i],extract_arguments(commandArgs()[argnr]),envir=.envir)
    }
  }
  if(!.silent) {
    print("",quote=FALSE)
    print("### READ COMMAND LINE - ASSIGNED CONFIGURATION ###",quote=FALSE)
    eprint_list(allowed_args, envir=.envir)
    print("### READ COMMAND LINE - CONFIGURATION END ###",quote=FALSE)
    print("",quote=FALSE)
  }
}
