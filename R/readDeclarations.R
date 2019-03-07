#' readDeclarations
#' 
#' Reads all declarations given in a GAMS code and returns them.
#' 
#' 
#' @param file A gams file or a vector containing GAMS code.
#' @param unlist A logical indicating whether the output should be returned as
#' a list separated by object type or a matrix.
#' @param types of declarations to be read.
#' @return Either a list of declared objects or a matrix containing object
#' name, the sets the object depends on and the description.
#' @author Jan Philipp Dietrich
#' @importFrom stringr str_sub str_extract str_replace_all str_replace fixed
#' @export
#' @seealso \code{\link{codeCheck}}
readDeclarations <- function(file, unlist=TRUE, types=c("scalar","(positive |)variable","parameter","table","equation","set")){
  if(length(file)==1) {
    if(file == "") return(NULL)
    file <- readLines(file,warn=FALSE)
  }
  d <- GAMScodeFilter(file)
  endings <- grep(";[   ]*(!!|$)",d)
  out <- list()
  for(t in types) {
   tname <- sub("\\([^\\)]*\\)","",t)
   if(tname=="table") tname<-"parameter"
   startings <- grep(paste("^ *",t,"[s]?( |$)",sep=""),d, ignore.case = TRUE)
   for(s in startings) {
     if(any(endings>=s)) {
       e <- min(endings[endings>=s])
     } else {
       e <- length(d)
     }
     tmp <- d[s:e] #cut all object declarations of the given type
     quoted_description <- str_sub(str_extract(tmp,"\"[^\"]*\""),2,-2) # extract quoted descriptions
     names(quoted_description) <- paste0("[#DESC#",1:length(tmp),"#]")
     tmp <- str_replace(tmp,"\"[^\"]*\"", names(quoted_description)) # insert placeholders
     tmp <- sub(";","",tmp) # remove ;
     tmp <- sub("^\\$.*","",tmp) # remove $-expressions
     tmp <- sub(paste("^ ?",t,"[^ ]*",sep=""),"",tmp, ignore.case = TRUE) # remove type name
     tmp <- sub("/.*/","",tmp) # remove / xyz / entries
     .rmFilling <- function(x){
       n <- grep("/",x)
       if(length(n)<2) return(x)
       rm <- NULL
       for(i in seq(1,length(n)-1,2)){
         rm <- c(rm,(n[i]+1):n[i+1])
         x[n[i]] <- sub("/.*$","",x[n[i]])
       }
       keep <- setdiff(1:length(x),rm)
       return(x[keep])
     }
     tmp <- .rmFilling(tmp)
     tmp <- gsub("\t"," ",tmp)
     tmp <- grep("^ *.{0,1} *$",tmp,invert=TRUE,value=TRUE) #remove all lines with no objects in them
     structure <- "^[ \t]*([^ ^,^\\(^\t]+)(\\([^\\)]+\\)|)[ \t]*(.*)$"
     names <- sub(structure,"\\1",tmp) #store name
     sets <- sub(structure,"\\2",tmp) #store sets
     sets <- gsub("\\(","",sets)
     sets <- gsub("\\)","",sets)
     sets <- gsub(" +","",sets)
     description <- sub("^ *$","",gsub(",","",sub(structure,"\\3",tmp))) #store description (remove "," and empty entries)
     description <- sub(" */.*$","", description) # remove values at the end, if given
     description <- str_replace_all(description,fixed(quoted_description))
     tmp <- cbind(names,sets,description)
     out[[tname]] <- rbind(out[[tname]],tmp)
   }
  }
  if(unlist) {
    tmp <- NULL
    for(n in names(out)) {
      tmp <- rbind(tmp,cbind(out[[n]],type=n))
    }
    out <- tmp
  }
  return(out)
}
