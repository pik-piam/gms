#' findCoupledruns
#' 
#' Extracts scenario names from coupled runs in the given outputfolder. The
#' scenario names will be extracted based on the folder names of the results
#' folders.
#' 
#' 
#' @usage findCoupledruns(resultsfolder)
#' @param resultsfolder Path to an output folder.
#' @return A vector containing the names of the scenarios.
#' @author David Klein
#' @export
findCoupledruns <- function(resultsfolder) {
  cat("\nNo run specified by user. Searching for all scenarios of coupled runs in",resultsfolder,": ")
  # Find which runs were performed by searching for all files that contain "-rem-"
  runs <- Sys.glob(path(resultsfolder,"/*-rem-*"))
  if (length(runs)==0) runs <- Sys.glob(path(resultsfolder,"/*-mag-*"))
  # keep directories only (filter out files)
  runs <- runs[file.info(runs)[,"isdir"]]
  # Remove "-rem-*" from the folder names and remove remaining double elements to yield the pure runname
  runs <- unique(sub("-(rem|mag)-[0-9]+","",runs))
  # Remove path information and leave only scenarioname
  runs <- sub(paste0(resultsfolder,"/"),"",runs)
  cat(length(runs)," scenarios found.\n")
  return(runs)
}
