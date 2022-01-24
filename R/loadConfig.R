#' Load Config
#'
#' Load config in YAML format as written via
#' \code{\link{saveConfig}}.
#'
#' @param cfg Either a character string naming a file which containes the config
#'            or a character string containing the config as YAML code.
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{saveConfig}}
#' @importFrom yaml read_yaml
#' @examples
#' cfg <- list(input = c(data1 = "blub.tgz", data2 = "bla.tgz"), mode = "default")
#' yml <- saveConfig(cfg)
#' loadConfig(yml)
#' @export
loadConfig <- function(cfg) {
  handlers <- list(namedVector = function(x) unlist(x),
                   character = function(x) as.character(x))
  if (length(cfg) == 1 && file.exists(cfg)) return(read_yaml(cfg, handlers = handlers))
  return(yaml.load(cfg, handlers = handlers))
}
