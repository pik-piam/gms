#' Save Config
#'
#' Saves config in YAML format so that it can be read in again via
#' \code{\link{loadConfig}}.
#'
#' @param cfg Input config which should be saved
#' @param file A character string naming a file. If set to NULL the YAML output
#' will be returned by the function.
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{loadConfig}}
#' @importFrom yaml write_yaml as.yaml
#' @examples
#' cfg <- list(input = c(data1 = "blub.tgz", data2 = "bla.tgz"), mode = "default")
#' saveConfig(cfg)
#' @export
saveConfig <- function(cfg, file = NULL) {
  yamlPrep <- function(cfg) {
    for (i in seq_along(cfg)) {
      if (is.list(cfg[[i]])) {
        cfg[[i]] <- yamlPrep(cfg[[i]])
      } else if (length(cfg[[i]]) > 1 && !is.null(names(cfg[[i]]))) {
        cfg[[i]] <- as.list(cfg[[i]])
        attr(cfg[[i]], "tag") <- "namedVector"
      } else if (length(cfg[[i]]) == 0 && is.character(cfg[[i]])) {
        attr(cfg[[i]], "tag") <- "character"
      }
    }
    return(cfg)
  }
  cfg <- yamlPrep(cfg)
  if (is.null(file)) return(as.yaml(cfg))
  write_yaml(cfg, file)
}
