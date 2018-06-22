#' readDeclarations
#' 
#' Reads all declarations given in a GAMS code and returns them.
#' 
#' 
#' @usage readDeclarations(file,unlist=TRUE)
#' @param file A gams file or a vector containing GAMS code.
#' @param unlist A logical indicating whether the output should be returned as
#' a list separated by object type or a matrix.
#' @return Either a list of declared objects or a matrix containing object
#' name, the sets the object depends on and the description.
#' @author Jan Philipp Dietrich
#' @export
#' @seealso \code{\link{codeCheck}}
readDeclarations <- function(file,unlist=TRUE){
  if(length(file)==1) {
    if(file == "") return(NULL)
    file <- readLines(file,warn=FALSE)
  }
  d <- GAMScodeFilter(file)
  endings <- grep(";[   ]*(!!|$)",d)
  types <- c("scalar","(positive |)variable","parameter","table","equation")
  out <- list()
  for(t in types) {
   tname <- sub("\\([^\\)]*\\)","",t)
   if(tname=="table") tname<-"parameter"
   startings <- grep(paste("^ *",t,"[s]?( |$)",sep=""),d, ignore.case = TRUE)
   for(s in startings) {
     e <- min(endings[endings>=s])
     tmp <- d[s:e] #cut all object declarations of the given type
     tmp <- sub(";","",tmp) # remove ;
     tmp <- sub("^\\$.*","",tmp) # remove $-expressions
     tmp <- sub(paste("^ ?",t,"[^ ]*",sep=""),"",tmp, ignore.case = TRUE) # remove type name
     tmp <- grep("^ *.{0,1} *$",tmp,invert=TRUE,value=TRUE) #remove all lines with no objects in them
     structure <- "^[ \t]*([^ ^,^\\(^\t]+)(\\([^\\)]+\\)|)[ \t]*(.*)$"
     names <- sub(structure,"\\1",tmp) #store name
     sets <- sub(structure,"\\2",tmp) #store sets
     sets <- gsub("\\(","",sets)
     sets <- gsub("\\)","",sets)
     sets <- gsub(" +","",sets)
     description <- sub("^ *$","",gsub(",","",sub(structure,"\\3",tmp))) #store description (remove "," and empty entries)
     description <- sub(" */.*$","", description) # remove values at the end, if given
     tmp <- cbind(names,sets,description)
     out[[tname]] <- rbind(out[[tname]],tmp)
   }
  }
  if(unlist) {
    tmp <- NULL
    for(n in names(out)) {
      #dimnames(out[[n]])[[1]] <- rep(n,dim(out[[n]])[1])
      tmp <- rbind(tmp,out[[n]])
    }
    out <- tmp
  }
  return(out)
}
