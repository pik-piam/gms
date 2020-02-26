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
#' @importFrom dplyr %>% group_by summarize arrange rename
#' @export

readRuntime <- function(path,plot=FALSE,types=NULL,coupled=FALSE,outfname=NULL) {
  run <- NULL
  runtime <- NULL
  maindir <- getwd()
  
  cat("\nReading runtime for",length(path),"runs\n")
  for (d in path) {
    splittedpath <- strsplit(d, "/")[[1]]
    runfolder <- splittedpath[length(splittedpath)]
    datafile <- paste0(d,"/runstatistics.rda")
    
    # try to read runtime data from runstatistics.rda
    tmp <- NULL
    start <- NULL
    end <- NULL
    if (!file.exists(datafile)) {
      cat("No file found ",datafile,"\n")
    } else if (file.info(datafile)$size==0) {
      cat("Empty file ",datafile,"\n")
    } else {
      # if file exists and it's file size is >0: load it
      stats <- NULL
      load(datafile)
      #  # read GAMS runtime from file
      #  if(!is.null(stats) & !is.null(stats$runtime)) {
      #    tmp <- stats$runtime
      #  } else {
      #    cat("No runtime information found in",datafile,"\n")
      #  }
      # read start end end of run from file
      if(!is.null(stats) & !is.null(stats$starttime)) {
        start <- stats$starttime
        end   <- stats$endtime
        #cat("Found runtime information in",datafile,"\n")
      }
    }
    
    # if no start and end was extractable from runstatistics.rda 
    # conclude it from timestamps of the files in the results folder
    if (is.null(start)) {
      setwd(d)
      # find all files
      info <- file.info(dir())
      # sort files in info by mtime
      info <- info[order(info$mtime),]
      # save time of first file in the list (oldest)
      start <- info[1,]$mtime
      # save time if last file in the list (newest)
      
      if ("report.rds" %in% rownames(info)) {
        # if run has finished normally the report.rds file should exist. In this case take the newest file
        end <- tail(info$mtime,n=1)
      } else {
        # if report.rds does not exist, this indicates that the run did not finish properly and the mif file has been generated manually later without also producing the report.rds
        # In this case do not take the newest file (which is the manually and belated produced mif file) but take the full.lst which is the newest file before the mif file
        cat("Using",runfolder,"full.lst as end\n")
        end <- info["full.lst",]$mtime
      }
      setwd(maindir)
    }
    
    # if runtime data was found
    if(!all(c(is.null(tmp),is.null(start),is.null(end)))) {
      # need to be transformed to NA otherwise rbind would not work if one of them is NULL
      tmp <- end-start
      units(tmp)="hours"
      if(is.null(start)) start <- NA
      if(is.null(end))   end   <- NA
      new <- data.frame(run=runfolder,type="NA",value=tmp,start=start,end=end,stringsAsFactors=FALSE)
      runtime <- rbind(runtime,new)
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
      p_iterations <- ggplot2::ggplot(data=runtime, ggplot2::aes_string(x="it", y="value", fill="type")) + 
        ggplot2::geom_bar(stat="identity", position=ggplot2::position_dodge()) +
        ggplot2::scale_x_continuous(breaks = runtime$it) + 
        ggplot2::facet_wrap(~run) + 
        ggplot2::scale_y_continuous() + 
        ggplot2::xlab("Coupling iteration") + 
        ggplot2::ylab("Hours") #+ ggplot2::theme(text = ggplot2::element_text(size = 28))
      lusweave::swfigure(out,print,p_iterations,sw_option="height=9,width=16")
    }
    
    # Order runs descending by runtime
    # step 1: calculate total runtime for each run and order descending (in new data frame)
    tot <- runtime %>% group_by(.data$run) %>% summarize(total = sum(.data$value)) %>% arrange(.data$total)
    # step 2: use the order of runs in this new data frame to order levels or "run" in runtime accordingly
    runtime$run <- ordered(factor(runtime$run),levels=tot$run) 
    
    # Plot: compare runs of all scenarios
    # Convert hours to days if total runtime is longer than 3 days
    if (max(tot$total,na.rm=TRUE) > 24*3) {
      y_unit <- "days"
      runtime$value <- runtime$value/24
    } else {
      y_unit <- "hours"
    }
    
    p_sorted <- ggplot2::ggplot(data=runtime, ggplot2::aes_string(x="run", y="value",fill=ifelse(is.null(types),"NULL","type"))) + 
      ggplot2::geom_bar(colour="black",stat="identity") + 
      ggplot2::coord_flip() +
      ggplot2::ylab(y_unit) + 
      ggplot2::ggtitle("Ordered by runtime") + 
      ggplot2::scale_y_continuous() +
      ggplot2::theme(text = ggplot2::element_text(size = 20))
    
    lusweave::swfigure(out,print,p_sorted,sw_option="height=9,width=16")
    
    # sort runs and levels by starttime
    dat <- runtime %>% arrange(.data$start) %>% mutate(run = factor(.data$run, levels=rev(unique(.data$run)), ordered=TRUE))
    
    p_timeline <- ggplot2::ggplot(dat, ggplot2::aes_string(color = ifelse(is.null(types),"NULL","type"))) +
      ggplot2::geom_segment(ggplot2::aes_string(x="start", xend="end", y="run", yend="run"), size=3) +
      ggplot2::ylab("") + 
      ggplot2::xlab("") + 
      ggplot2::theme(legend.position = c(.95, .9)) #+ 
      #ggplot2::scale_x_datetime(labels = date_format("%H:%M\n%b %d", tz="CET")) #, date_minor_breaks = "2 hours")
    
    lusweave::swfigure(out,print,p_timeline,sw_option="height=9,width=16")
    
    # Calculate and display statistics    
    dat <- dat %>% rename(duration = .data$value, model = .data$type)
    x <- as.numeric(max(dat$end)-min(dat$start),units="hours")
    #cat("From start to end:\n  ",round(x),"hours\n")
    lusweave::swlatex(out,paste0("From start to end:\\newline  ",round(x)," hours\\newline\\newline"))
    
    #cat("Average runtime:","\n")
    lusweave::swlatex(out,"Average runtime:\\newline")
    x <- dat %>% group_by(.data$model) %>% summarize(duration_single_mean = mean(.data$duration))
    for (m in x$model) {
      #cat("  ",m, round(x[x$model==m,]$duration_single_mean,1),"hours \n")
      lusweave::swlatex(out,paste0(" ",m," ",round(x[x$model==m,]$duration_single_mean,1)," hours\\newline" ))
    }
    
    x <- dat %>% group_by(.data$run) %>% summarize(duration_coupled_mean = sum(.data$duration)) %>% summarize(duration_mean = mean(.data$duration_coupled_mean))
    #cat("   Coupled: ",round(as.numeric(x$duration_mean,units="hours"),1),"hours\n")
    lusweave::swlatex(out,paste0("coupled ",round(as.numeric(x$duration_mean,units="hours"),1)," hours\\newline\\newline"))
    
    #cat("Number of runs:\n")
    lusweave::swlatex(out,"Number of runs:\\newline")
    x <- dat %>% group_by(.data$model) %>% summarize(no_of_runs = length(.data$model))
    for (m in x$model) {
      #cat("  ",m, x[x$model==m,]$no_of_runs,"\n")
      lusweave::swlatex(out,paste0(m," ",x[x$model==m,]$no_of_runs,"\\newline"))
    }
    lusweave::swlatex(out,"\\newline")
    
    #cat("Total runtime:\n")
    lusweave::swlatex(out,"Total runtime:\\newline")
    x <- dat %>% group_by(.data$model) %>% summarize(total_time = sum(.data$duration))
    for (m in x$model) {
      #cat("  ",m, round(as.numeric(x[x$model==m,]$total_time,units="days"),1),"days\n")
      lusweave::swlatex(out,paste0(m," ",round(as.numeric(x[x$model==m,]$total_time,units="days"),1)," days\\newline"))
    }
    #cat("   Total:",round(sum(as.numeric(x$total_time,units="days")),1),"days\n")
    lusweave::swlatex(out,paste0(" Total ",round(sum(as.numeric(x$total_time,units="days")),1)," days\\newline"))
    
    if (is.null(outfname)) outfname <- "runtime"
    lusweave::swclose(out,outfile=paste0(outfname,".pdf"),clean_output=FALSE)
  }
  invisible(res)
}
