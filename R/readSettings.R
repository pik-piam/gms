#' readSettings
#'
#' Reads all settings (parameters, globals, and their respective values)
#' in the given GAMS code.
#'
#'
#' @param file A gams file or a vector containing GAMS code.
#' @return A vector of parameter values and their names.
#' @author Mika Pflüger
#' @export
#' @seealso \code{\link{readDeclarations}}
readSettings <- function(file) {
  d <- GAMScodeFilter(readFileOrVector(file))
  parameterNames <- readDeclarations(d, types = c("parameter"))[, "names"]
  parameters <- readParameterValues(d, parameterNames)
  globals <- readSetglobals(d)
  return(append(parameters, globals))
}
