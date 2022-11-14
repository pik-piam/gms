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

  mainFilePath <- file.path(modelpath, mainfile)
  withr::with_dir(modelpath, {
    system2("gams", c(mainFilePath, "action=c", "dumpopt=21"))
  })

  dumpFilePath <- file.path(modelpath, paste0(substr(mainfile, 1, nchar(mainfile) - 4), ".dmp"))
  file.copy(dumpFilePath, output)
}
