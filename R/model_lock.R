#' Model lock/unlock
#'
#' Lock the model to coordinate multiple processes accessing the model.
#' This is necessary if you change files within the model and have to
#' avoid that another process reads from your half-written changes in
#' the mean time.
#' model_lock creates an exclusive lock on the given file and returns
#' a lock id. When the lock id is handed to model_unlock the lock is
#' dropped again, and another process can take the lock. The lock
#' is also dropped when the lock id goes out of scope and is garbage
#' collected in R or when the R process terminates or crashes. However,
#' it is good practice to explicitly call model_unlock when you don't
#' need the lock anymore.
#' If you want to check for informational purposes if the model is
#' currently locked by another process, you can use the is_model_locked()
#' function.
#' Note however that you should never use is_model_locked() to coordinate
#' access to the model folder. The return value of is_model_locked() is
#' only a snapshot at the time when the function is run.
#' Note additionally that is_model_locked() checks if the model is
#' locked by another process. If you currently hold the lock of the model
#' yourself, it will return FALSE.
#'
#'
#' @aliases model_lock model_unlock is_model_locked
#' @usage model_lock(folder = ".", file = ".lock", timeout1 = 12, timeout2 = NULL,
#' check_interval = NULL, oncluster = NULL)
#' model_unlock(id, folder = NULL, file = NULL, oncluster = NULL)
#' is_model_locked(folder = ".", file = ".lock")
#' @param folder path to the model folder
#' @param file file name of the lock file. Don't use the lock file for anything
#' else (i.e. don't read or write or open the lock file).
#' @param timeout1 Time in hours to wait for other processes to free the lock
#' before giving up and exiting with an error.
#' @param timeout2 deprecated setting which will be ignored.
#' @param check_interval deprecated setting which will be ignored.
#' @param id lock id as returned by model_lock.
#' @param oncluster deprecated setting which will be ignored.
#' @return model_lock returns the lock id which is needed
#' to identify the lock in model_unlock.
#' @author Jan Philipp Dietrich, David Klein, Mika Pflüger
#' @export
#' @importFrom filelock lock
#' @seealso \code{\link{check_config}}
#' @examples
#' #lock folder
#' id <- model_lock(tempdir())
#'
#' #unlock folder
#' model_unlock(id)
model_lock <- function(folder=".", file=".lock", timeout1=12, timeout2=NULL, check_interval=NULL, oncluster=NULL) {

  if (!is.null(timeout2)) warning("timeout2 setting is deprecated and ignored.")
  if (!is.null(check_interval)) warning("check_interval setting is deprecated and ignored.")
  if (!is.null(oncluster)) warning("oncluster setting is deprecated and ignored.")

  lfile <- file.path(folder, file)
  size <- file.size(lfile)
  if (!is.na(size) & size != 0) {
    # locking file exists and has content - only possible when the old locking code was / is in use
    stop(sprintf("model_lock: found old locking file with content. Likely, old (pre 2022-06) locking was or is in use.
Check if all runs using old locking are finished, and if they are, remove the lock file %s.", lfile))
  }

  # filelock::lock does not respect umask settings for access rights when creating files on unix.
  # Therefore, if no lock file exists, create it first.
  # This is racy because the lock is not taken yet and checking the existence and creating the file is not done in one
  # step; however, that is not problem since touching a file multiple times works.
  if (is.na(size) & .Platform$OS.type == "unix") {
    system2(c("touch", lfile))
  }

  # lock takes the timeout in milliseconds, timeout1 is in hours.
  timeStartLock <- Sys.time()
  message(timeStartLock, ": try to acquire model lock...")
  id <- lock(lfile, timeout = timeout1 * 3600000)
  if (is.null(id)) {
    # timeout
    stop(sprintf(
      "model_lock: could not acquire lock within timeout1 = %s h. Check what holds the lock or increase timeout1.",
      timeout1)
    )
  }

  timeLocked <- Sys.time()
  timediff <- timeLocked - timeStartLock
  message(timeLocked, ": acquired model lock in ", round(timediff, 1), " ", units(timediff), ".")

  return(id)
}
