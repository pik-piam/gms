#' readDefaultConfig
#'
#' Reads the default configuration of the model. Uses default.cfg or main.cfg as the source as appropriate.
#' To read a configuration from YAML format, use \code{\link{loadConfig}} instead.
#'
#'
#' @param path path of the main folder of the model
#' @return A vector of parameter values and their names.
#' @author Mika Pflüger
#' @seealso \code{\link{loadConfig}}
#' @importFrom withr with_environment
#' @importFrom utils modifyList
#' @export

readDefaultConfig <- function(path) {
  # read in settings from main.gms first
  cfg <- list()
  fileMain <- file.path(path, "main.gms")
  if (file.exists(fileMain)) {
    cfg$gms <- as.list(readSettings(fileMain))
  } else {
    stop("readDefaultConfig cannot find main.gms in ", normalizePath(path))
  }
  # overwrite with settings from default.cfg
  env <- new.env()
  fileDefault <- file.path(path, "config", "default.cfg")
  if (file.exists(fileDefault)) {
    source(fileDefault, local = env)  # nolint: undesirable_function_linter
  } else {
    stop("readDefaultConfig cannot find config/default.cfg in ", normalizePath(path))
  }
  return(modifyList(cfg, env$cfg, keep.null = TRUE))
}
