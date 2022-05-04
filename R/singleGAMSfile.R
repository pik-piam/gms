#' Merge GAMS code into single file
#'
#' This function merges GAMS code which is distributed over severals files into
#' a single GAMS file. Optionally, it also embeds R scripts into the single GAMS
#' file
#'
#'
#' @param modelpath The path where the model is stored
#' @param mainfile The path to the main gams file (relative to the model path)
#' @param output Name of the single output GAMS file.
#' @param embedRScripts If TRUE, R scripts called by GAMS via Execute are also embedded. Default FALSE
#' @author Jan Philipp Dietrich, Anastasis Giannousakis
#' @export
#' @importFrom utils tail
#' @examples
#' # copy dummymodel create single gms file out of it
#' file.copy(system.file("dummymodel", package = "gms"), tempdir(), recursive = TRUE)
#' model      <- paste0(tempdir(), "/dummymodel")
#' singlefile <- paste0(tempdir(), "/full.gms")
#' singleGAMSfile(modelpath = model, output = singlefile)
#'
singleGAMSfile <- function(modelpath = ".", mainfile = "main.gms", output = "full.gms", embedRScripts = FALSE) {

  .insertIncludeFile <- function(code, i, path) {
    path <- gsub(";", "", path)
    if (file.exists(path)) {
      includeFileContent <- suppressWarnings(readLines(path))
      if (i < length(code)) {
        remainder <- code[(i + 1):length(code)]
      } else {
        remainder <- NULL
      }
      code <- c(code[1:(i - 1)], paste0("*", code[i], " DONE!"), includeFileContent, remainder)
    } else {
      stop("Include file ", path, " could not be found!")
    }
    return(code)
  }

  .mergeGamsFiles <- function(mainfile) {
    code <- readLines(mainfile, warn = FALSE)
    code <- c("* #### CODE MERGED WITH FUNCTION gms::singleGAMSfile ####", "", code)

    setglobals <- list()
    i <- 1
    repeat {
      # find first include batinclude or setglobal command which was not handled yet
      i <- grep("^([^\\*].*\\$|\\$)((bat)?include|setglobal)", tail(code, -i), ignore.case = TRUE)[1] + i
      if (is.na(i)) break

      # check what type of command it is
      if (length(grep("^\\$setglobal", code[i], ignore.case = TRUE)) == 1) {
        # store global variable in setglobals list
        tmp <- strsplit(code[i], " +")[[1]]
        if (length(grep("%", tmp[3])) == 1) {
          setglobals[[tmp[2]]] <- batincludeArguments[as.integer(sub("%", "", tmp[3]))]
        } else {
          setglobals[[tmp[2]]] <- tmp[3]
        }
      } else if (length(grep("^\\$include", code[i], ignore.case = TRUE)) == 1) {
        p <- gsub("\\\"", "", strsplit(code[i], " +")[[1]][2])
        code <- .insertIncludeFile(code, i, p)
      } else if (length(grep("^\\$if exist.*\\$include", code[i], ignore.case = TRUE)) == 1) {
        p <- gsub("\\\"", "", strsplit(code[i], " +")[[1]][5])
        if (file.exists(p)) {
          code <- .insertIncludeFile(code, i, p)
        } else {
          code[i] <- paste0("*", code[i], " CONDITION WAS NEGATIVE!")
        }
      } else if (length(grep("^\\$batinclude", code[i], ignore.case = TRUE)) == 1) {
        tmp <- strsplit(code[i], " +")[[1]]
        p <- gsub("\\\"", "", tmp[2])
        batincludeArguments <- tail(tmp, -2)
        code <- .insertIncludeFile(code, i, p)
      } else if (length(grep("^\\$if setglobal", code[i], ignore.case = TRUE)) == 1) {
        tmp <- strsplit(code[i], " +")[[1]]
        var <- tmp[3]
        if (var %in% names(setglobals)) {
          p <- gsub("\\\"", "", tmp[5])
          code <- .insertIncludeFile(code, i, p)
        } else {
          code[i] <- paste0("*", code[i], " CONDITION WAS NEGATIVE!")
        }
      } else if (length(grep("^\\$(Ifi|if)", code[i], ignore.case = TRUE)) == 1) {
        tmp <- strsplit(code[i], " +")[[1]]
        var <- gsub("(\\\"|%)", "", tmp[2])
        val <- gsub("(\\\"|%)", "", tmp[4])
        if (is.null(setglobals[[var]])) {
          warning("No value set for $setglobal \"", var, "\"! Value is set to \"MISSING\"!")
          setglobals[[var]] <- "MISSING"
        }
        if (setglobals[[var]] == val) {
          p <- gsub("\\\"", "", tmp[6])
          code <- .insertIncludeFile(code, i, p)
        } else {
          code[i] <- paste0("*", code[i], " CONDITION WAS NEGATIVE!")
        }
      } else {
        warning("Catched a command which could not be translated (", code[i], ")")
      }
    }
    return(code)
  }

  .embedRScripts <- function(code) {
    i <- 1
    repeat {
      pattern <- "^(execute(.async)?(.checkErrorLevel)?)\\s+\"Rscript (.*)\";"
      # find first execute command for an Rscript which was not handled yet
      i <- grep(pattern, tail(code, -i), ignore.case = TRUE)[1] + i
      if (is.na(i)) break

      # extract R script file name and execute command
      executeCommand <- sub(pattern, "\\1", code[i], ignore.case = TRUE)
      rFileName <- sub(pattern, "\\4", code[i], ignore.case = TRUE)
      # check if R script is included in core or a module
      if ((substr(rFileName, 0, 4) == "core" | substr(rFileName, 0, 7) == "modules") & file.exists(rFileName)) {
        # embed the R script using $onecho and $offecho
        # we also replace the "Execute" call so that it finds the script at the position it is written by $onecho
        newRFileName <- gsub("[\\/]", "_", rFileName)
        rFileContent <- suppressWarnings(readLines(rFileName))
        if (i < length(code)) {
          remainder <- code[(i + 1):length(code)]
        } else {
          remainder <- NULL
        }
        code <- c(code[1:(i - 1)],
                  paste0("$onecho > ", newRFileName),
                  rFileContent,
                  "$offecho",
                  paste0(executeCommand, " \"Rscript ", newRFileName, "\";"),
                  remainder)
        i <- i + length(rFileContent) + 2
      }
      else {
        # R script wasn't found at the expected location, can't be embedded
        i <- i + 1
      }
    }
    return(code)
  }

  withr::with_dir(modelpath, {
    # set LC_ALL to C to avoid locale warnings
    Sys.setlocale("LC_ALL", "C")
    code <- .mergeGamsFiles(mainfile)
    if (embedRScripts) {
      code <- .embedRScripts(code)
    }
  })

  writeLines(code, output)
}
