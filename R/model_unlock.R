#' @export
model_unlock <- function(id,folder=".",file=".lock",oncluster=NULL) {
  
  if(!is.null(oncluster)) warning("oncluster setting is deprecated and ignored.")
  
  lfile <- path(folder,file)
  if(!file.exists(lfile)) stop("Lock file does not exist!")
  lock_queue <- .readLock(lfile)
  lock_queue <- lock_queue[lock_queue$id!=id,]
  if(nrow(lock_queue)>0) {
    .writeLock(lock_queue,lfile)
    message("...model run with id ",id," removed from queue!")
  } else {
    unlink(lfile)
    message("...model unlocked!")
  }
}