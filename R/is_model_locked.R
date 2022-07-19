#' @importFrom filelock lock unlock
#' @export
is_model_locked <- function(folder = ".", file = ".lock") {
  # Check if old locking file (with content) is in use
  lfile <- file.path(folder, file)
  if (! file.exists(lfile)) {
    return(FALSE)
  }
  size <- file.size(lfile)
  if (!is.na(size) & size != 0) {
    # old locking file in use, locked
    message("is_model_locked: old (pre 2022-06) locking in use, assume locked.")
    return(TRUE)
  }

  # TODO: use filelock function when https://github.com/r-lib/filelock/issues/23 is closed
  # Until it exists, we try to acquire the lock with a short timeout to check if it is
  # locked currently. We release it immediately afterwards, but if multiple processes
  # are checking at the same time, results might be inconsistent.
  id <- lock(lfile, timeout = 1)
  if (is.null(id)) {
    # lock could not be acquired
    return(TRUE)
  } else {
    # lock was acquired, release it and report unlocked
    unlock(id)
    return(FALSE)
  }
}
