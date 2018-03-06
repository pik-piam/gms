#' goxygen
#' 
#' Documentation function which extracts a full model documentation from a 
#' modularized gams model. (incomplete --- work in progress!) 
#' 
#' @param path path to the model to be documented
#' @param docfolder folder the documentation should be written to
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{codeCheck}}
#' @export


goxygen <- function(path=".", docfolder="doc") {
  if (!requireNamespace("knitr", quietly = TRUE)) stop("The package knitr is not available! Install it first before running goxygen!")

  if(is.character(path)) {
    cc <- codeCheck(path=path, debug=TRUE)
  } else {
    cc <- path
  }
  if(!dir.exists(docfolder)) dir.create(docfolder, recursive = TRUE)
  cwd <- getwd()
  on.exit(setwd(cwd))
  setwd(docfolder)
  
  collectTables <- function(cc) {
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
    out <- list()
    for(m in names(cc$interfaceInfo)) {
      out[[m]] <- interfaceTables(cc,m)
      out[[m]]$declarations <- moduleTables(cc,m)
    }
    return(out)
  }
  
  writeModulePage <- function(name,data) {
    zz <- file(paste0(name,".md"), "w")
    
    .empty <- function(zz) {
      writeLines("",zz)
    }
    
    .write <- function(data,zz) {
      if(!is.null(data)) writeLines(data,zz)
      .empty(zz)
    }
    
    .header <- function(title,level,zz) {
      if(level<3) {
        writeLines(title,zz)
        symbol <- ifelse(level==1,"=","-")
        writeLines(paste(rep(symbol,nchar(title)), collapse=""),zz)
      } else {
        start <- paste(rep("#",level),collapse="")
        writeLines(paste(start,title),zz)
      }
      .empty(zz)
    }
    
    
    .header(name,1,zz)
    .header("Description",2,zz)
    .header("Interfaces",2,zz)
    .header("Input",3,zz)
    .write(data$input, zz)
    
    .header("Output",3,zz)
    .write(data$output, zz)
    
    .header("Interface plot",3,zz)
    
    .header("Realizations",2,zz)
    
    .header("Definitions",2,zz)
    .write(data$declarations, zz)
    
    .header("Developers",2,zz)
    
    .header("See Also",2,zz)
    
    .header("References",2,zz)
    
    close(zz)
  }
   
  out <- collectTables(cc)

  # write doc files
  for(m in names(out)) {
    writeModulePage(m,out[[m]])
  }
  
  return(out)
}