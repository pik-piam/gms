#' readRuntime
#' 
#' Reads all runtime information from given experiments. The runtime is given
#' in hours and is the runtime of GAMS.
#' 
#' 
#' @usage readRuntime(path,plot=FALSE,types=NULL,coupled=FALSE,outfname=NULL)
#' @param path Path to a run or a vector of paths.
#' @param plot Logical indicating whether the output should be plotted to a pdf
#' with the name runtime.pdf.
#' @param types A vector of names of different types of runs which should be
#' distinguished by colors. The names have to be part of the folder name in
#' order to allow the function to map the given types to the runs.
#' @param coupled Logical indicating if comparison plots should be added for
#' coupled REMIND and MAgPIE runs. \code{TRUE} automatically sets \code{types}
#' to \code{c("-rem-","-mag-")} and overwrites user defined \code{types}
#' @param outfname Optional name of the pdf. If nothing is given the default
#' "runtime" will be used.
#' @return A data frame containing the run names and runtime information in
#' hours.
#' @author David Klein
#' @importFrom rlang .data
#' @importFrom dplyr %>% group_by summarize arrange
#' @export

readRuntime <- function(path,plot=FALSE,types=NULL,coupled=FALSE,outfname=NULL) {
  run <- NULL
  runtime <- NULL
  cat("\nReading runtime for",length(path),"runs\n")
  for (d in path) {
    splittedpath <- strsplit(d, "/")[[1]]
    runfolder <- splittedpath[length(splittedpath)]
    datafile <- paste0(d,"/runstatistics.rda")
    if (!file.exists(datafile)) {
      cat("No file found ",datafile,"\n")
    } else if (file.info(datafile)$size==0) {
      cat("Empty file ",datafile,"\n")
    } else {
      # if file exists and it's file size is >0: load it
      stats <- NULL
      load(datafile)
      tmp <- NULL
      if(!is.null(stats) & !is.null(stats$runtime)) {
        tmp <- stats$runtime
      } else {
        cat("No runtime information found in",datafile,"\n")
      }
      
      # if runtime data was found
      if(!is.null(tmp)) {
        units(tmp) <- "hours"
        tmp <- as.double(tmp)
        new <- data.frame(run=runfolder,type="NA",value=tmp,stringsAsFactors=FALSE)
        runtime <- rbind(runtime,new)
      }
    }
  }
  
  if (coupled) types <- c("-rem-","-mag-")

  # define "types"-column
  if(!is.null(types)) {
    for(tt in types) runtime$type[grep(tt,runtime$run)] <- tt
  }
  
  # cosmetics: order levels for better order in plots (starting with remind) and remove "-"
  if (identical(types,c("-rem-","-mag-"))) {
    runtime$type <- sub("(-)(rem|mag)(-)","\\2",runtime$type) 
    runtime$type <-ordered(factor(runtime$type),levels=c("rem","mag")) 
  }

  if(is.null(runtime)) {warning("No runtime information found for all runs!")}
  res <- runtime #save runtime for returning it before it is modified below

  # generate plots
  if (plot) {
    cat("\nPreparing pdf with runtime plots.\n")
    out<-lusweave::swopen(template="david")
    if (coupled) {
      # change runnames
      itnumber <- sub(".*-([0-9]{1,2}$)","\\1",runtime$run) # replace everything ".*" with pattern contained in brackets
      runname  <- sub("-(rem|mag)(-[0-9]{1,2}$)","",runtime$run) # replace pattern contained in brackets with nothing
      
      # replace runnames
      runtime$run <- runname
      
      # add column with iteration number, convert to numeric for sorting on x-axis in plot
      runtime$it <- itnumber
      runtime$it <- as.numeric(runtime$it)
      
      # plot all runs into one plot
      p0 <- ggplot2::ggplot(data=runtime, ggplot2::aes_string(x="it", y="value", fill="type")) + ggplot2::geom_bar(stat="identity", position=ggplot2::position_dodge()) +
          ggplot2::scale_x_continuous(breaks = runtime$it) + ggplot2::facet_wrap(~run) +
          ggplot2::xlab("Coupling iteration") + ggplot2::ylab("Hours") + ggplot2::theme(text = ggplot2::element_text(size = 28))
      lusweave::swfigure(out,print,p0,sw_option="height=9,width=16")
    }

    # Order runs descending by runtime
    # step 1: calculate total runtime for each run and order descending (in new data frame)
    tot <- runtime %>% group_by(.data$run) %>% summarize(total = sum(.data$value)) %>% arrange(.data$total)
    # step 2: use the order of runs in this new data frame to order levels or "run" in runtime accordingly
    runtime$run <- ordered(factor(runtime$run),levels=tot$run) 

    # Plot: compare runs of all scenarios
    # Convert hours to days if total runtime is longer than 3 days
    if (max(tot$total) > 24*3) {
      y_unit <- "days"
      runtime$value <- runtime$value/24
    } else {
      y_unit <- "hours"
    }

    p1 <- ggplot2::ggplot(data=runtime, ggplot2::aes_string(x="run", y="value",fill=ifelse(is.null(types),"NULL","type"))) + ggplot2::geom_bar(colour="black",stat="identity") + ggplot2::coord_flip() +
      ggplot2::ylab(y_unit) + ggplot2::theme(text = ggplot2::element_text(size = 20)) + ggplot2::ggtitle("Ordered by runtime")

    lusweave::swfigure(out,print,p1,sw_option="height=9,width=16")
    
    if (is.null(outfname)) outfname <- "runtime"
    lusweave::swclose(out,outfile=paste0(outfname,".pdf"),clean_output=TRUE)
  }
  return(res)
}
