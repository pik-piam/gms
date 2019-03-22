#' is.modularGAMS
#' 
#' Checks whether a folder seems to contain modular GAMS code or not. 
#' 
#' @param path path to the main folder of the model
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{codeCheck}}
#' @export
#' @examples
#' is.modularGAMS()
#' 
is.modularGAMS <- function(path=".") {
  cwd <- getwd()
  setwd(path)
  on.exit(setwd(cwd))
  if(!file.exists("main.gms")) return(FALSE)
  if(!dir.exists("modules")) return(FALSE)
  return(TRUE)
}