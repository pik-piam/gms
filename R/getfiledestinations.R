#' getfiledestinations
#'
#' Create file2destination mapping based on information from the model, ignoring
#' top-level directories listed in `.gitignore`.
#'
#' @param path main model folder
#' @param ignoreFolders folders to be ignored by the function, additionally to
#' directories listed in `.gitignore` (by default only "renv").
#' @md
#' @export
#'
#' @author Jan Philipp Dietrich, David Klein

getfiledestinations <- function(path = ".", ignoreFolders = "renv") {
  folders <- base::list.dirs(path = path, recursive = FALSE, full.names = FALSE)

  if (0 == file.access(file.path(path, ".gitignore"), mode = 4)) {
    ignores <- grep("/[:space:]*$",               # directories end on "/"
                    sub("(^|[^\\\\])#.*$", "\\1", # strip comments
                        readLines(file.path(path, ".gitignore"))),
                    value = TRUE)
    ignores <- basename(ignores)[dirname(ignores) %in% c(".", "/")]

    # turn globs into regex patterns
    globs <- grep("\\*", ignores, value = TRUE)
    globs <- gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", globs)
    globs <- gsub("\\\\\\*", ".*", globs)

    ignores  <- c(gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1",
                       grep("\\*", ignores, value = TRUE, invert = TRUE)),
                  globs)
  } else {
    ignores <- character()
  }

  folders <- grep(paste0("^(\\.|(", paste(ignores, collapse = "|"), ")$)"),
                  folders, value = TRUE, invert = TRUE)
  folders <- setdiff(folders, ignoreFolders)
  files <- NULL
  for (f in folders) files <- c(files, dir(path = file.path(path, f), pattern = "^files$", recursive = TRUE, full.names = TRUE))
  out <- NULL
  for (f in files) {
    tmp <- grep("^\\*", readLines(f, warn = FALSE), invert = TRUE, value = TRUE)
    add <- data.frame(file = tmp, destination = dirname(f), stringsAsFactors = FALSE)
    out <- rbind(out, add)
  }
  if (is.null(out)) {
    return(NULL)
  }
  out <- as.data.frame(lapply(out, trimws), stringsAsFactors = FALSE)
  return(out[out[[1]] != "", ])
}
