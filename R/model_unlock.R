#' @export
model_unlock <- function(id, folder=".", file=".lock", oncluster=NULL) {

  if (!is.null(oncluster)) warning("oncluster setting is deprecated and ignored.")

  lfile <- path(folder,file)
  explainStop <- paste0("This can mean that two or more processes changed files in the folder at the same time.\n",
     "Settings or initial values may be incorrect in all of them.\n",
     "Carefully check the other runs whose model locks and unlocks have timestamps overlapping with this run.\n",
     "Current time is ", Sys.time(), ".")
  if (!file.exists(lfile)) stop("Lock file ", file, " does not exist!\n", explainStop)
  lock_queue <- .readLock(lfile)
  if (! id %in% lock_queue$id) stop("Lock file ", file, " does not contain id = ", id, "!\n", explainStop)
  lock_queue <- lock_queue[lock_queue$id != id, ]
  if (nrow(lock_queue) > 0) {
    .writeLock(lock_queue, lfile)
    message("...model run with id = ", id, " removed from queue at ", Sys.time(), ".")
  } else {
    unlink(lfile)
    message("...model unlocked at ", Sys.time(), ".")
  }
}
