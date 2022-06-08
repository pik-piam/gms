#' Publish data in a repository
#'
#' Downloads a list of tgz files from a list of repos, merge them and publish it on another server
#'
#' @param input a vector of files to be downloaded or a cfg list with settings to be used (e.g. containing
#' cfg$input, cfg$repositories). Settings in the config list will be overwritten by other arguments of
#' this function if they are not set to NULL
#' @param name name of the data to be published (will be used in as file name). If no name is given (default) source
#' files will be published as is (separate tgz files with original name).
#' @param target target the data should be published in (format user@server:/folder/path) If a target vector, or targets
#' separated by "|" are provided the user will be asked interactively where the file should be written to.
#' By default it will look for target information in the environment variable PUBLISH_DATA_TARGET
#' @param ... further options provided to \code{\link{download_unpack}}
#' @seealso \code{\link{download_unpack}},\code{\link{tardir}}
#' @author Jan Philipp Dietrich
#' @importFrom utils untar
#' @importFrom withr with_connection with_dir local_tempdir
#' @export

publish_data <- function(input, name = NULL, target = Sys.getenv("PUBLISH_DATA_TARGET", unset = "."), ...) { # nolint
  merge <- !is.null(name) # only unpack and merge files if name is given

  if (length(target) == 1) target <- strsplit(target, "|", fixed = TRUE)[[1]]

  if (length(target) > 1) {
    chooseTarget <- function(target, title = "Please choose target") {
      message("\n\n", title, ":\n")
      message(paste(seq_along(target), target, sep = ": ", collapse = "\n"))
      message("\nNumber: ")
      identifier <- getLine()
      identifier <- as.numeric(strsplit(identifier, ",")[[1]])
      if (any(!(identifier %in% seq_along(target)))) {
        stop("This choice (", identifier, ") is not possible. Please type in a number between 1 and ", length(target))
      }
      return(target[identifier])
    }
    target <- chooseTarget(target)
  }

  # filter target repositories from source repository
  if (is.list(input)) {
    tmp <- grep(strsplit(target, ":")[[1]][2], names(input$repositories), value = TRUE, invert = TRUE)
    input$repositories <- input$repositories[tmp]
  }
  tmpdir <- local_tempdir()
  dir <- file.path(tmpdir, "data")
  dir.create(dir)
  download_unpack(input, targetdir = dir, unpack = merge, ...)
  if (merge) {
    tmptarfiles <- paste0(sub("\\.tgz$", "", name), ".tgz")
    folder <- tmpdir
    tardir(dir = dir, tarfile = paste0(folder, "/", tmptarfiles))
  } else {
    folder <- dir
    tmptarfiles <- list.files(dir)
  }

  if (grepl(":", target)) {
    with_dir(folder, {
      system(paste0("sftp ", target, " <<< $'mput *.tgz'"))
      unlink("*.tgz")
    })
  } else {
    for (tmptarfile in tmptarfiles) {
      file.copy(paste0(folder, "/", tmptarfile), paste0(normalizePath(target), "/", tmptarfile))
      unlink(paste0(folder, "/", tmptarfile))
    }
  }
}
