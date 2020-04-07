#' checkSwitchAppearance
#' 
#' Checks for all declared switches in which parts of the model they appear and
#' calculates the type of each object (core object, interface object, module
#' object of module xy,...)
#' 
#' 
#' @param code Model code returned by \code{\link{codeExtract}}
#' @return A list with three elements: switches, appearance and type. Switches
#' is a vector containing all switches. The names of the vector contain the
#' information where the switch is set. Appearance is a matrix containing
#' values which indicate whether an object appears in a part of the code or not
#' (e.g. indicates whether "vm_example" appears in realization "on" of module
#' "test" or not.). 0 means that it does not appear, 1 means that it appears in
#' the code and 2 means that it appears in the not_used.txt. Type is a vector
#' containing the type of each object.
#' @author Jan Philipp Dietrich
#' @export
#' @seealso
#' \code{\link{codeCheck}},\code{\link{readDeclarations}},\code{\link{codeExtract}},\code{\link{checkAppearance}}


checkSwitchAppearance <- function(code){
  code <- grep("^\\$",code,value=TRUE) #only use lines which start with a $
  code <- grep("^\\$(include|ondelim|offdelim|ifthen)",code,value=TRUE,invert=TRUE, ignore.case = TRUE) #remove lines which cannot contain switches
  code <- grep("\\$if (not |)setglobal",code,value=TRUE,invert=TRUE, ignore.case = TRUE) #remove lines containing the structure "if not"
  setglobal <- grep("setglobal",code,value=TRUE, ignore.case = TRUE)
  pattern <- "^\\$setglobal +([^ ]*)( +.*|)$"
  switches <- sub(pattern,"\\1",setglobal, ignore.case = TRUE)
  tmp_func <- function(name,x) { return(paste(x[names(x)==name],collapse=" "))}
  tmp <- sapply(unique(names(code)),tmp_func,code)
  a <- t(sapply(paste("(^|[^[:alnum:]_])",escapeRegex(unique(switches)),"($|[^[:alnum:]_])",sep=""),grepl,tmp))
  if (length(tmp) == 1) a<-t(a)
  dimnames(a)[[1]] <- unique(switches)
  dimnames(a)[[2]] <- names(tmp)
  
  tmp <- dimnames(a)[[1]]
  tmp2 <- grep("_",tmp,invert=TRUE)
  tmp[tmp2] <- paste("_",tmp[tmp2],sep="")
  type <- sub("^(o|c|)[^_]*?(m|[0-9]{2}|)_.*$","\\1\\2",tmp)
  names(type) <- dimnames(a)[[1]]
  return(list(switches=switches,appearance=a,type=type))
}