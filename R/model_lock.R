#' Model lock/unlock
#'
#' Functions that indicate whether a model folder is currently locked by
#' another process or not. This helps to prevent unintended interactions
#' between processes.
#'
#'
#' @aliases model_lock model_unlock
#' @usage model_lock(folder=".", file=".lock", timeout1=12, timeout2=24,
#' check_interval=1, oncluster=NULL)
#' model_unlock(id,folder=".",file=".lock",oncluster=NULL)
#' @param folder model folder
#' @param file file name of the lock file containing the process queue
#' @param timeout1 Time in hours the top process in the queue is allowed to run
#' before the current process is stopped.
#' @param timeout2 Time in hours the processed is allowed to wait in the queue
#' before it is stopped
#' @param check_interval Time in seconds between checking the current position
#' in the queue.
#' @param id process id as returned by model_lock.
#' @param oncluster deprecated setting which will be ignored.
#' @return model_lock returns the process id which is needed
#' to identify the process in model_unlock.
#' @author Jan Philipp Dietrich, David Klein
#' @export
#' @seealso \code{\link{check_config}}
#' @examples
#' #lock folder
#' id <- model_lock(tempdir())
#'
#' #unlock folder
#' model_unlock(id,tempdir())
model_lock <- function(folder=".", file=".lock", timeout1=12, timeout2=24, check_interval=1, oncluster=NULL) {

  if (!is.null(oncluster)) warning("oncluster setting is deprecated and ignored.")

  .cleanQueue <- function(lfile) {
    lock_queue <- .readLock(lfile)
    outdated <- (lock_queue$auto_unlock < Sys.time())
    if (any(outdated)) {
      # removed outdated entries
      lock_queue <- lock_queue[!outdated, ]
      # recalculate auto unlock time for first in Queue, if it is a new entry
      # (as auto unlock time of running process depends on timeout1 setting,
      # while unlock time for queued processes depends on timeout2)
      if (outdated[1] & !all(outdated)) {
        lock_queue$auto_unlock[1] <- Sys.time() + lock_queue$timeout1[1] * 3600
      }
    }
    .writeLock(lock_queue, lfile)
  }

  lfile <- path(folder, file)
  if (file.exists(lfile)) {
    lock_queue <- .readLock(lfile)

    if (nrow(lock_queue) == 0) {
      id <- 1
      lock_queue <- data.frame(id = id, auto_unlock = Sys.time() + timeout1 * 3600, timeout1 = timeout1)
    } else {
      #create id
      id <- max(lock_queue$id) + 1
      # set unlock time based on timeout2
      auto_unlock <- Sys.time() + timeout2 * 3600 + 1
      #add entry to queue
      lock_queue <- rbind(lock_queue,
                          data.frame(id = id,
                                     auto_unlock = auto_unlock,
                                     timeout1 = timeout1))
    }
    .writeLock(lock_queue, lfile)
    .cleanQueue(lfile)
    #wait for being the first in the queue
    message("The model is already locked. Run queued in file ", file, " with id = ", id, " at ", Sys.time(), "...")
    repeat {
      if (lock_queue$id[1] == id) {
        message("...waiting finished. Starting model at ", Sys.time(), ".")
        break
      }
      if (!(id %in% lock_queue$id)) stop("Process removed from queue at ", Sys.time(),
                                         " (probable reason: timeout2 limit exceeded)!")
      Sys.sleep(check_interval)
      .cleanQueue(lfile)
      lock_queue <- .readLock(lfile)
    }
  } else {
    id <- 1
    lock_queue <- data.frame(id=id, auto_unlock = Sys.time() + timeout1 * 3600, timeout1 = timeout1)
    .writeLock(lock_queue, lfile)
    message("No queue, computation can continue. ",
            "Using file ", file, ", model has been locked with id = ", id, " at ", Sys.time(), ".")
  }
  return(id)
}

.readLock <- function(lfile) {
  lock_queue <- read.csv(lfile, stringsAsFactors = FALSE)
  lock_queue$auto_unlock <- as.POSIXct(lock_queue$auto_unlock)
  return(lock_queue)
}
.writeLock <- function(lock_queue, lfile) {
  write.csv(lock_queue, file = lfile, row.names = FALSE, quote = FALSE)
}
