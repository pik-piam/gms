#' readDefaultConfig
#'
#' Reads the default configuration of the model. Uses default.cfg or main.cfg as the source as appropriate.
#'
#'
#' @param path path of the main folder of the model
#' @return A vector of parameter values and their names.
#' @author Mika Pflüger
#' @importFrom withr with_environment
#' @importFrom utils modifyList
#' @export

readDefaultConfig <- function(path) {
  # read in settings from main.gms first
  cfg <- list()
  cfg$gms <- as.list(readSettings(paste0(path, "/", "main.gms")))
  # overwrite with settings from default.cfg
  env <- new.env()
  source(paste0(path, "/", "config/default.cfg"), local = env)  # nolint: undesirable_function_linter
  return(modifyList(cfg, env$cfg, keep.null = TRUE))
}
