#' selectScript
#'
#' Functions which allows for interactive selection of scripts/files.
#'
#'
#' @param folder Folder in which the files/scripts are located which should
#' be selected from.
#' @param ending File ending of the files to be selected (without dot)
#' @return A vector of paths to files selected by the user
#' @author Jan Philipp Dietrich
#' @importFrom yaml read_yaml yaml.load
#' @importFrom utils capture.output
#' @export

selectScript <- function(folder = ".", ending = "R") { # nolint

  width <- 70

  .printInfo <- function(info, startNumber = 1, width = 50, maxchar = 30) {
    descriptionWidth <- width - maxchar - 7
    info$position <- NULL
    if (!is.null(info$folder)) info$script <- info$folder
    info$number <- seq_len(nrow(info)) +  startNumber - 1
    info$number <- paste0(format(info$number, width = 2, justify = "right"), ": ")
    splitDescription <- strwrap(info$description, width = descriptionWidth, simplify = FALSE)
    out <- NULL
    for (i in seq_len(nrow(info))) {
      nExtra <- length(splitDescription[[i]]) - 1
      out <- rbind(out, data.frame(script = c(info$script[i], rep("", nExtra)),
                                   description = splitDescription[[i]],
                                   number = c(info$number[i], rep("    ", nExtra))))
    }
    message(paste0(out$number, format(gsub("_", " ", out$script, fixed = TRUE), width = maxchar, justify = "right"),
                   " | ", out$description, collapse = "\n"))
    return(out)
  }

  maxNchar <- function(x, width, prefix = "-> ", suffix = " <-", sep = "-") {
    width <- width - 7
    x <- capture.output(cat(strsplit(x, " ")[[1]], fill = width))
    x <- format(x, width = width, justify = "centre")
    if (!is.null(sep)) {
      sep <- paste0(" ", paste(rep(sep, nchar(prefix) + nchar(suffix) + nchar(x)[1]), collapse = ""))
    }
    return(paste(sep, paste0(" ", prefix, x, suffix, collapse = "\n"), sep, sep = "\n"))
  }

  subfolders <- list.dirs(folder, full.names = FALSE)
  subfolders <- subfolders[subfolders != ""]

  fp <- paste0("\\.", ending, "$")
  script <- gsub(fp, "", grep(fp, list.files(folder), value = TRUE))
  if (length(script) == 0 && length(subfolders) == 0) {
    message("Empty folder selected. NULL returned.")
    return(NULL)
  }

  subinfo <- data.frame(folder = NULL, description = NULL, position = NULL, stringsAsFactors = FALSE)
  for (s in subfolders) {
    if (file.exists(file.path(folder, s, "INFO.yml"))) {
      tmp <- read_yaml(file.path(folder, s, "INFO.yml"))
    } else {
      tmp <- list()
    }
    subinfo <- rbind(subinfo, data.frame(folder      = s,
                                        description = ifelse(is.null(tmp$description), "", tmp$description),
                                        position    = ifelse(is.null(tmp$position), NA, tmp$position),
                                        stringsAsFactors = FALSE))

  }
  if (nrow(subinfo) > 0) {
    subinfo <- subinfo[order(subinfo$position), ]
  }

  # read descriptions in scripts
  info <- data.frame(script = NULL, description = NULL, position = NULL, stringsAsFactors = FALSE)
  for (s in script) {
    tmp <- read_yaml_header(file.path(folder, paste0(s, ".", ending)))
    if (!is.null(tmp$error)) {
      tmp$description <- paste(tmp$description, "ERROR:", tmp$error)
    }
    info <- rbind(info, data.frame(script      = s,
                                  description = ifelse(is.null(tmp$description), "", tmp$description),
                                  position    = ifelse(is.null(tmp$position), NA, tmp$position),
                                  stringsAsFactors = FALSE))
  }
  if (nrow(info) > 0) {
    info <- info[order(info$position), ]
  }
  maxchar <- max(nchar(c(info$script, subinfo$folder)))

  if (file.exists(file.path(folder, "INFO.yml"))) {
    yaml <- read_yaml(file.path(folder, "INFO.yml"))
  } else {
    yaml <- list()
  }

  if (is.null(yaml$type)) yaml$type  <- "script"
  message("")
  if (!is.null(yaml$description)) message(yaml$description)
  if (!is.null(yaml$note)) message(maxNchar(yaml$note, width = width))
  .printInfo(info, maxchar = maxchar, width = width)
  if (nrow(subinfo) > 0) {
    message("\nAlternatively, choose a ", yaml$type, " from another selection:")
    .printInfo(subinfo, startNumber = nrow(info) + 1, maxchar = maxchar, width = width)
    message("Choose a ", yaml$type, " or folder: ", appendLF = FALSE)
  } else {
    message("Choose a ", yaml$type, ": ", appendLF = FALSE)
  }
  identifier <- getLine()
  identifier <- as.numeric(strsplit(identifier, ",")[[1]])
  if (all(identifier == 0)) return(NULL)
  if (any(!(identifier %in% 1:(nrow(info) + nrow(subinfo))))) {
    stop("This choice (", identifier, ") is not possible. Please type in a number between 1 and ",
         (nrow(info) + nrow(subinfo)))
  }
  if (any(identifier > nrow(info))) {
    folderIdentifier <- identifier[identifier > nrow(info)]
    identifier <- identifier[identifier <= nrow(info)]
  }
  if (length(identifier) > 0) {
    out <- paste0(info$script[identifier], ".", ending)
  } else {
    out <- NULL
  }
  if (exists("folderIdentifier")) {
    for (fi in folderIdentifier) {
      subfolder <- subinfo$folder[fi - nrow(info)]
      out <- c(out, file.path(subfolder, selectScript(file.path(folder, subfolder), ending = ending)))
    }
  }
  if (length(out) == 0) return(NULL)
  return(out)
}
