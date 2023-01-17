#' readParameterValues
#'
#' Read values of given parameters from a given GAMS code section.
#'
#' The GAMS code section should contain statements of the form
#' parameter = value;
#' for all the given parameters.
#'
#' @param code A vector with GAMS code.
#' @param parameters A vector with GAMS parameter names
#' @return A vector of values the parameters are set to with parameter names as
#' names.
#' @author Mika Pflüger
#' @seealso \code{\link{readDeclarations}}
readParameterValues <- function(code, parameters) {
  result <- vector(mode = "character")
  resultNames <- vector(mode = "character")
  for (parameter in parameters) {
    pattern <- paste0(".*\\b", parameter, "[ \\t]*=[ \\t]*([^; \\t]+);.*")
    matching <- grep(pattern, code, value = TRUE, perl = TRUE)
    values <- sub(pattern, "\\1", matching, perl = TRUE)
    if (length(values) == 0) {
      warning(paste0("Could not find value for ", parameter, ", skipping it."))
      next
    }
    if (length(values) > 1) {
      warning(paste0("Found multiple values for ", parameter, ", skipping it."))
      next
    }
    result <- append(result, values)
    resultNames <- append(resultNames, parameter)
  }
  names(result) <- resultNames
  return(result)
}
