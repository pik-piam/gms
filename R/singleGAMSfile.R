#' Merge GAMS code into single file
#' 
#' This function merges GAMS code which is distributed over severals files into
#' a single GAMS file.
#' 
#' 
#' @param modelpath The path where the model is stored
#' @param mainfile The path to the main gams file (relative to the model path)
#' @param output Name of the single output GAMS file.
#' @author Jan Philipp Dietrich, Anastasis Giannousakis
#' @export
#' @importFrom utils tail
#' @examples
#' # copy dummymodel create single gms file out of it
#' file.copy(system.file("dummymodel",package="gms"),tempdir(), recursive = TRUE)
#' model      <- paste0(tempdir(),"/dummymodel")
#' singlefile <- paste0(tempdir(),"/full.gms")
#' singleGAMSfile(modelpath=model, output=singlefile)
#' 

singleGAMSfile <- function(modelpath=".",mainfile="main.gms",output="full.gms") {

  cwd <- getwd()
  on.exit(setwd(cwd))
  setwd(modelpath)
  
  .insertIncludeFile <- function(code,i,path) {
    path <- gsub(";","",path)
    if(file.exists(path)) {
      tmp <- suppressWarnings(readLines(path))
      code <- c(code[1:(i-1)],paste("*",code[i]," DONE!",sep=""),tmp,code[(i+1):length(code)])
    } else {
      stop("Include file ", path, " could not be found!")
    }
    return(code)
  }
  
  #set LC_ALL to C to avoid locale warnings
  Sys.setlocale('LC_ALL','C') 
  
  code <- readLines(mainfile,warn = FALSE)
  code <- c("* #### CODE MERGED WITH FUNCTION gms::singleGAMSfile ####","",code)
  
  setglobals <- list()
  i <- 1
  repeat {
    #find first include batinclude or setglobal command which was not handled yet
    i <- grep("^([^\\*].*\\$|\\$)((bat)?include|setglobal)",tail(code,-i),ignore.case=TRUE)[1] + i
    if(is.na(i)) break
    
    #check what type of command it is
    if(length(grep("^\\$setglobal",code[i],ignore.case=TRUE))==1) {
      #store global variable in setglobals list
      tmp <- strsplit(code[i]," +")[[1]]  
      if(length(grep("%",tmp[3]))==1) {
        setglobals[[tmp[2]]] <- arguments[as.integer(sub("%","",tmp[3]))] 
      } else {
        setglobals[[tmp[2]]] <- tmp[3]
      }     
    } else if(length(grep("^\\$include",code[i],ignore.case=TRUE))==1) {
      p <- gsub("\\\"","",strsplit(code[i]," +")[[1]][2])
      code <- .insertIncludeFile(code,i,p)      
    } else if(length(grep("^\\$batinclude",code[i],ignore.case=TRUE))==1) {
      tmp <- strsplit(code[i]," +")[[1]]
      p <- gsub("\\\"","",tmp[2])
      arguments <- tail(tmp,-2)
      code <- .insertIncludeFile(code,i,p)
    } else if(length(grep("^\\$if setglobal",code[i],ignore.case=TRUE))==1) {
      tmp <- strsplit(code[i]," +")[[1]]
      var <- tmp[3]
      if(var %in% names(setglobals)) {
        p <- gsub("\\\"","",tmp[5])    
        code <- .insertIncludeFile(code,i,p)
      } else {
        code[i] <- paste("*",code[i]," CONDITION WAS NEGATIVE!",sep="")
      }
    } else if(length(grep("^\\$(Ifi|if)",code[i],ignore.case=TRUE))==1) {
      tmp <- strsplit(code[i]," +")[[1]]
      var <- gsub("(\\\"|%)","",tmp[2])
      val <- gsub("(\\\"|%)","",tmp[4])
      if(is.null(setglobals[[var]])) {
        warning("No value set for $setglobal \"",var,"\"! Value is set to \"MISSING\"!")
        setglobals[[var]] <- "MISSING"   
      }
      if(setglobals[[var]]==val) {
        p <- gsub("\\\"","",tmp[6])    
        code <- .insertIncludeFile(code,i,p)
      } else {
        code[i] <- paste("*",code[i]," CONDITION WAS NEGATIVE!",sep="")
      }
    } else {
      warning("Catched a command which could not be translated (",code[i],")")
    }
  }
 setwd(cwd)
 writeLines(code,output)  
}
