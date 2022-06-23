#' @importFrom filelock unlock
#' @export
model_unlock <- function(id, folder=NULL, file=NULL, oncluster=NULL) {

  if (!is.null(folder)) warning("model_unlock: folder setting is deprecated and ignored.")
  if (!is.null(file)) warning("model_unlock: file setting is deprecated and ignored.")
  if (!is.null(oncluster)) warning("oncluster setting is deprecated and ignored.")

  unlock(id)
  message(Sys.time(), ": unlocked model.")
}
