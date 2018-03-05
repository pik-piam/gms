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
  
  .merge <- function(dec) {
    # merge information
    pattern <- "^(.*) \\((.*)\\) *(|\\/.*\\/ *)$"
    description <- sub(pattern,"\\1",dec[,"description"])
    unit <- sub(pattern,"\\2",dec[,"description"])
    unit[!grepl(pattern,dec[,"description"])] <- ""
    unit <- sub("mio.","10^6",unit)
    return(data.frame(name=dec[,"names"],sets=dec[,"sets"], description=description, unit=unit, stringsAsFactors = FALSE))
  }
  
  .format <- function(out,aps) {
    # format information
    fout <- data.frame(Name=paste0(out$name,sub("()","",paste0("(",out$sets,")"),fixed=TRUE)), 
                       Description=out$description, 
                       Unit=out$unit)
    aps[,] <- ifelse(aps==1,"x","")
    return(cbind(fout,aps))
  }
  
  .clean <- function(x,caption) {
    if(nrow(x)==0) return(NULL)
    rownames(x) <- NULL
    return(knitr::kable(x, "pandoc", caption=caption))
  }
  
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
  
    out  <- .merge(dec)
    fout <- .format(out,aps)
      
    out <- list(input=fout[ifs[names(ifs) == "in"],],output=fout[ifs[names(ifs) == "out"],1:3])
    
    out$input  <- .clean(out$input,"input")
    out$output <- .clean(out$output,"output")
    return(out)
  }
  
  moduleTables <- function(cc, module) {
    # collect information about module interfaces
    dec <- cc$gams$declarations[grepl(paste0("^",module,"\\."),rownames(cc$gams$declarations)),, drop=FALSE]
    dec <- dec[order(dec[,"names"]),]
    if(nrow(dec)==0) return(NULL)
    dec <- dec[!(dec[,"names"] %in% cc$interfaceInfo[[module]]),, drop=FALSE]
    dec <- dec[!duplicated(dec[,"names"]),,drop=FALSE]
    
    aps <- cc$ap$appearance[dec[,"names"],grepl(paste0("^",module,"\\."),colnames(cc$ap$appearance)),drop=FALSE]
    colnames(aps) <- sub("^.*\\.","",colnames(aps))
    aps <- aps[!duplicated(rownames(aps)),,drop=FALSE]
    
    out <- .merge(dec)
    out <- .format(out,aps)
    return(.clean(out,"declarations"))
  }
  
  if(is.character(path)) {
    cc <- codeCheck(path=path, debug=TRUE)
  } else {
    cc <- path
  }

  out <- list()
  for(m in names(cc$interfaceInfo)) {
    out[[m]] <- interfaceTables(cc,m)
    out[[m]]$declarations <- moduleTables(cc,m)
  }
  return(out)
}