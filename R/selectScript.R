selectScript <- function(folder=".", title="Please choose an outputmodule", ending="R") {
  
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
  
  order <- function() {
    forder <- file.path(folder,"order.cfg")
    if(file.exists(forder)) {
      order <- grep("(#|^$)",readLines(forder),invert=TRUE,value=TRUE)
      if(length(order)==0) order <- NULL
    } else {
      order <- NULL
    }
    return(order)  
  }
  
  fp <- paste0("\\.",ending,"$")
  
  script <- gsub(fp,"",grep(fp,list.files(folder), value=TRUE))
  
  #read descriptions in scripts
  info <- data.frame(script=NULL,desc=NULL, stringsAsFactors = FALSE)
  #sort modules based on order.cfg
  script <- intersect(union(order(),script),script)
  
  for(s in script) {
    tmp <- readLines(file.path(folder,paste0(s,".",ending)))
    desc <- paste(sub("^#: ","",(grep("^#: ",tmp,value=TRUE))), collapse="\n")
    info <- rbind(info,data.frame(script=s,desc=ifelse(length(desc)==0,"",paste(" |",desc)), stringsAsFactors = FALSE))
  }
  maxchar <- max(nchar(info$script))
  
  message(title,":")
  message(paste0(1:nrow(info), ": ", format(gsub("_"," ",info$script,fixed=TRUE), width=maxchar, justify="right"), info$desc, collapse="\n"))
  message("Number: ",appendLF = FALSE)
  identifier <- get_line()
  identifier <- as.numeric(strsplit(identifier,",")[[1]])
  if (any(!(identifier %in% 1:length(script)))) stop("This choice (",identifier,") is not possible. Please type in a number between 1 and ",length(module))
  return(paste0(script[identifier],".",ending))
}