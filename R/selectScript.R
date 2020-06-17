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

selectScript <- function(folder=".", ending="R") {
  
  get_line <- function(){
    # gets characters (line) from the terminal or from a connection
    # and returns it
    if(interactive()){
      s <- readline()
    } else {
      con <- file("stdin")
      s <- readLines(con, 1, warn=FALSE)
      on.exit(close(con))
    }
    return(s);
  }
  
  max_nchar <- function(x,width,prefix="-> ", suffix=" <-", sep="-") {
    x <- capture.output(cat(strsplit(x," ")[[1]], fill=width))
    x <- format(x,width=width , justify = "centre")
    if(!is.null(sep)) {
      sep <- paste0(" ",paste(rep(sep,nchar(prefix)+nchar(suffix)+nchar(x)[1]),collapse=""))
    }
    return(paste(sep,paste0(" ",prefix,x,suffix, collapse="\n"),sep,sep="\n"))
  }

  subfolders <- list.dirs(folder,full.names = FALSE)
  subfolders <- subfolders[subfolders!=""]
  
  fp <- paste0("\\.",ending,"$")
  script <- gsub(fp,"",grep(fp,list.files(folder), value=TRUE))
  if(length(script)==0 && length(subfolders)==0) {
    message("Empty folder selected. NULL returned.")
    return(NULL)
  }

  subinfo <- data.frame(folder=NULL,description=NULL, position=NULL, stringsAsFactors = FALSE)
  for(s in subfolders) {
    if(file.exists(file.path(folder,s,"INFO.yml"))) {
      tmp <- read_yaml(file.path(folder,s,"INFO.yml"))
    } else {
      tmp <- list()
    }
    subinfo <- rbind(subinfo,data.frame(folder      = s,
                                        description = ifelse(is.null(tmp$description),"",tmp$description),
                                        position    = ifelse(is.null(tmp$position),NA,tmp$position), 
                                        stringsAsFactors = FALSE))
    
  }
  if(nrow(subinfo)>0) subinfo <- subinfo[order(subinfo$position),]
  
  #read descriptions in scripts
  info <- data.frame(script=NULL,description=NULL, position=NULL, stringsAsFactors = FALSE)
  for(s in script) {
    tmp <- read_yaml_header(file.path(folder,paste0(s,".",ending)))
    info <- rbind(info,data.frame(script      = s,
                                  description = ifelse(is.null(tmp$description),"",tmp$description),
                                  position    = ifelse(is.null(tmp$position),NA,tmp$position), 
                                  stringsAsFactors = FALSE))
  }
  if(nrow(info)>0) info <- info[order(info$position),]
  maxchar <- max(nchar(c(info$script,subinfo$folder)))
  
  if(file.exists(file.path(folder,"INFO.yml"))) {
    yaml <- read_yaml(file.path(folder,"INFO.yml"))
  } else {
    yaml <- list()
  }

  if(is.null(yaml$type)) yaml$type  <- "script"
  message("")
  if(!is.null(yaml$description)) message(yaml$description)
  if(!is.null(yaml$note)) message(max_nchar(yaml$note,width=40))
  message(paste0(format(1:nrow(info),width=2, justify="right"), ": ", format(gsub("_"," ",info$script,fixed=TRUE), width=maxchar, justify="right")," | ", info$description, collapse="\n"))
  if(nrow(subinfo)>0) {
    message("\nAlternatively, choose a ",yaml$type," from another selection:")
    message(paste0(format(nrow(info)+(1:nrow(subinfo)),width=2, justify="right"),": ", format(subinfo$folder, width=maxchar, justify="right"), " | ", subinfo$description, collapse="\n"))
  }
  message("Choose a ",yaml$type,": ",appendLF = FALSE)
  identifier <- get_line()
  identifier <- as.numeric(strsplit(identifier,",")[[1]])
  if(all(identifier==0)) return(NULL)
  if (any(!(identifier %in% 1:(nrow(info)+nrow(subinfo))))) stop("This choice (",identifier,") is not possible. Please type in a number between 1 and ",(nrow(info)+nrow(subinfo)))
  if(any(identifier>nrow(info))) {
    folder_identifier <- identifier[identifier>nrow(info)]
    identifier <- identifier[identifier<=nrow(info)]
  }
  if(length(identifier)>0) {
    out <- paste0(info$script[identifier],".",ending)
  } else {
    out <- NULL
  }
  if(exists("folder_identifier")) {
    for(fi in folder_identifier) {
      subfolder <- subinfo$folder[fi-nrow(info)]
      out <- c(out, file.path(subfolder,selectScript(file.path(folder,subfolder), ending=ending)))
    }
  }
  if(length(out)==0) return(NULL)
  return(out)
}