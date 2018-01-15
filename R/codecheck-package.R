#' PIK code checking and manipulation library for R
#' 
#' A library containing code checking and manipulation routines mostly for GAMS
#' code but also for other languages
#' 
#' \tabular{ll}{ Package: \tab codecheck\cr Type: \tab Package\cr Version: \tab
#' 0.05\cr Date: \tab 2012-05-22\cr License: \tab LGPL-3\cr LazyLoad: \tab
#' yes\cr }
#' 
#' @name codecheck-package
#' @aliases codecheck-package codecheck
#' @docType package
#' @author Jan Philipp Dietrich
#' 
#' Maintainer: Jan Philipp Dietrich <dietrich@@pik-potsdam.de>
#' @seealso ~~ \code{\link{codeCheck}} ~~
#' @keywords package
#' @examples
#' 
#' \dontrun{codeCheck("trunk_magpie")}
#' 
NULL


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