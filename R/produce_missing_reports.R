#' Produces the reporting mif files where they are missing
#' 
#' For coupled runs: Searches the output folder for all existing run folders,
#' checks which of them are currently running on the cluster, ignores them, 
#' checks for the remaining runs whether there is a reporting. Produces the 
#' reporting if it is missing.
#' #' 
#' @param modeldir Path to the main folder of REMIND or MAgPIE.
#' @importFrom utils read.delim
#' @author David Klein
#' @export

produce_missing_reports <- function(modeldir="./") {
  library(lucode)
  
  maindir <- getwd()
  on.exit(setwd(maindir))
  setwd(modeldir)
  
  allruns <- findIterations(findCoupledruns("output"),"output",latest=TRUE)
  running <- system("squeue -u $USER -o \"%Z %j\"",intern=TRUE)
  running <- read.delim(text=running,sep=" ",stringsAsFactors=FALSE)
  
  remove <- NULL
  for (r in running$NAME) {
    # find running among all runs
    tmp <- allruns[grepl(paste0(r,"-(rem|mag)-"),allruns)]
    remove <- c(remove,tmp)
  }
  
  # remove running runs from all runs
  check_for_report <- setdiff(allruns,remove)
  
  produce_report <- NULL
  
  for (r in check_for_report) {
    # check if report is available
    mag <- file.exists(paste0(r,"/report.mif"))
    rem <- Sys.glob(paste0(r,"/REMIND_generic_*.mif"))
    if (identical(character(0),rem)) {
      rem <- FALSE
    } else {
      rem <- file.exists(rem)
    }
    if (!mag & !rem) {
      #cat("No report found for",r,"\n")
      produce_report <- c(produce_report,r)
    }
  }
  
  if (is.null(produce_report)) {
    cat("No missing reports found.\n")
  } else {
    cat("Missing reports found for\n")
    print(produce_report)
    sw <- readline("Do you want to produce the output for these runs now? (y/n): ")
    if(sw=="y") {
      #MAGPIE
      comp <- FALSE
      submit <- "direct"
      outputdirs <- produce_report
      #REMIND
      outputdir <- produce_report
      
      #REMIND & MAGPIE
      output <- c("report","reporting")
      
      source_include <- TRUE
      sys.source("output.R",envir=new.env())
    }
  }
}

