#' goxygen
#' 
#' Documentation function which extracts a full model documentation from a 
#' modularized gams model. (incomplete --- work in progress!) 
#' 
#' @param path path to the model to be documented
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{codeCheck}}
#' 


goxygen <- function(path=".") {
  if (!requireNamespace("knitr", quietly = TRUE)) stop("The package knitr is not available! Install it first before running goxygen!")
  
  interfaceTables <- function(cc, module) {
    # collect information about module interfaces
    ifs <- cc$interfaceInfo[[module]]
    ifs <- sort(ifs)
    dec <- cc$gams$declarations[cc$gams$declarations[,"names"] %in% ifs,, drop=FALSE]
    dec <- dec[!duplicated(dec[,"names"]),,drop=FALSE]
    aps <- cc$ap$appearance[ifs,grepl(paste0("^",module,"\\."),colnames(cc$ap$appearance)),drop=FALSE]
    colnames(aps) <- sub("^.*\\.","",colnames(aps))
    aps <- aps[dec[,"names"],,drop=FALSE]
    aps <- aps[!duplicated(rownames(aps)),,drop=FALSE]
    
    # merge information
    pattern <- "^(.*) \\((.*)\\) *(|\\/.*\\/ *)$"
    description <- sub(pattern,"\\1",dec[,"description"])
    unit <- sub(pattern,"\\2",dec[,"description"])
    unit[!grepl(pattern,dec[,"description"])] <- ""
    unit <- sub("mio.","10^6",unit)
    out <- data.frame(name=dec[,"names"],sets=dec[,"sets"], description=description, unit=unit, stringsAsFactors = FALSE)
  
    # format information
    fout <- data.frame(Name=paste0(out$name,sub("()","",paste0("(",out$sets,")"),fixed=TRUE)), 
                       Description=out$description, 
                       Unit=out$unit)
    aps[,] <- ifelse(aps==1,"x","")
    fout <- cbind(fout,aps)
    
    out <- list(input=fout[ifs[names(ifs) == "in"],],output=fout[ifs[names(ifs) == "out"],1:3])
    
    .tmp <- function(x,caption) {
      if(nrow(x)==0) return(NULL)
      rownames(x) <- NULL
      return(knitr::kable(x, "pandoc", caption=caption))
    }
    out$input <- .tmp(out$input,"input")
    out$output <- .tmp(out$output,"output")
    return(out)
  }
  
  if(is.character(path)) {
    cc <- codeCheck(path=path, debug=TRUE)
  } else {
    cc <- path
  }

  out <- list()
  for(m in names(cc$interfaceInfo)) {
    out[[m]] <- interfaceTables(cc,m)
  }
  return(out)
}