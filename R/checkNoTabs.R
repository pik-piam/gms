#' checkNoTabs
#'
#' Check all files (also in subdirectories) matching the given pattern for tabs.
#' Will throw an error with a list of files where tabs were found if any.
#'
#' @param pattern A regular expression. Only files matching this pattern will be checked for tabs.
#' @param exclude A regular expression. Files matching this pattern will never be checked.
#' @param excludeFolders Paths to folders that should not be checked.
#' @return Invisibly, the list of files that were checked.
#'
#' @author Pascal Sauer
#' @examples
#' \dontrun{
#' gms::checkNoTabs(pattern = "\\.(R|Rprofile|gms|cfg|bib)$",
#'                  excludeFolders = c("output", "renv", ".git"))
#' gms::checkNoTabs(utils::glob2rx("*.R"))
#' }
#' @export
checkNoTabs <- function(pattern, exclude = NULL, excludeFolders = NULL) {
  folders <- normalizePath(list.dirs(".", recursive = FALSE))
  if (!is.null(excludeFolders)) {
    folders <- setdiff(folders, normalizePath(excludeFolders, mustWork = FALSE))
  }
  filesToCheck <- c(list.files(folders, pattern = pattern,
                               all.files = TRUE, full.names = TRUE, recursive = TRUE),
                    list.files(".", pattern = pattern,
                               all.files = TRUE, full.names = TRUE, recursive = FALSE))
  if (!is.null(exclude)) {
    filesToCheck <- grep(exclude, filesToCheck, invert = TRUE, value = TRUE)
  }

  filesWithTabs <- Filter(x = filesToCheck, f = containsTab)

  if (length(filesWithTabs) > 0) {
    stop("Please replace tabs with spaces in the following files:",
         paste("\n-", filesWithTabs, collapse = ""))
  }
  return(invisible(filesToCheck))
}

containsTab <- function(filePath) {
  return(any(grepl("\t", readLines(filePath))))
}
