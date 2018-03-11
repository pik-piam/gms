#' goxygen
#' 
#' Documentation function which extracts a full model documentation from a 
#' modularized gams model. (incomplete --- work in progress!) 
#' 
#' @param path path to the model to be documented
#' @param docfolder folder the documentation should be written to relative to model folder
#' @author Jan Philipp Dietrich
#' @importFrom stringi stri_extract_all_regex stri_replace_all_regex
#' @seealso \code{\link{codeCheck}}
#' @export


goxygen <- function(path=".", docfolder="doc") {
  if (!requireNamespace("pander", quietly = TRUE)) stop("The package pander is not available! Install it first before running goxygen!")
  cwd <- getwd()
  on.exit(setwd(cwd))
  
  if(is.character(path)) {
    setwd(path)
    cc <- codeCheck(debug=TRUE)
  } else {
    cc <- path
  }
  
  if(!dir.exists(docfolder)) dir.create(docfolder, recursive = TRUE)
  
  setwd(docfolder)
  
  collectTables <- function(cc) {
    .merge <- function(dec) {
      # merge information
      pattern <- "^(.*) \\((.*)\\) *(|\\/.*\\/ *)$"
      description <- sub(pattern,"\\1",dec[,"description"])
      unit <- sub(pattern,"\\2",dec[,"description"])
      unit[!grepl(pattern,dec[,"description"])] <- ""
      unit <- sub("mio.","10^6",unit)
      unit <- gsub("\\\\","/",unit)
      return(data.frame(name=dec[,"names"],sets=dec[,"sets"], description=description, unit=unit, stringsAsFactors = FALSE))
    }
    .format <- function(out,aps) {
      # format information
      fout <- data.frame(Name=paste0(out$name,sub("()","",paste0(" (",out$sets,")"),fixed=TRUE)), 
                         Description=out$description, 
                         Unit=out$unit)
      aps[,] <- ifelse(aps==1,"x","")
      return(cbind(fout,aps))
    }
    .clean <- function(x,caption) {
      if(nrow(x)==0) return(NULL)
      rownames(x) <- NULL
      return(pander::pandoc.table.return(x, "pandoc", caption=caption, split.table=160))
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
      
      out$input  <- .clean(out$input,"module inputs")
      out$output <- .clean(out$output,"module outputs")
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
      return(.clean(out,"module-internal objects"))
    }

    modInfo <- rbind(cc$modulesInfo,core=c(name="core",number="",folder="core",realizations=""))
    out <- list()
    for(m in names(cc$interfaceInfo)) {
      out[[modInfo[m,"folder"]]] <- interfaceTables(cc,m)
      out[[modInfo[m,"folder"]]]$declarations <- moduleTables(cc,m)
    }
    return(out)
  }
  
  escapeRegex <- function(x) {
    return(gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", x))
  }
  
  extractModuleDescription <- function(path, comment="*'") {
    comment <- paste0("^",escapeRegex(comment)," *")
    x <- readLines(path, warn = FALSE)
    x <- grep(comment,x,value=TRUE)
    x <- sub(comment,"",x)
    while(length(x)>2 & x[2]=="") x <- x[-2]
    out <- list(title=x[1],description=x[-1])
    if(is.na(out$title)) out$title <- "*TITLE MISSING*"
    if(length(out$description)==0) out$description <- "> *DESCRIPTION MISSING*"
    return(out)
  }
  
  extractRealization <- function(path, comment="*'") {
    x <- readLines(path, warn = FALSE)
    x <- grep("^\\*[^']",x,value=TRUE,invert=TRUE)
    x <- paste(x,collapse="\n")
    equation <- "(^|\n).*\\.\\.(.|\n)*?;"
    eq <- stri_extract_all_regex(x,equation)[[1]]
    eq <- gamsequation2tex(eq)
    
    x <- stri_replace_all_regex(x,equation,paste(comment,"\n",comment,"#::.equation.::#","\n",comment,"\n"))
    co <- stri_extract_all_regex(x,paste0(escapeRegex(comment),".*(\\n|$)"))[[1]]
    co <- gsub(paste0("(\n|",escapeRegex(comment)," *)"),"",co)
    
    # fill in equations
    for(i in names(eq)) {
      delim <- ifelse(grepl("CONVERSION FAILED!",i,fixed = TRUE), "```","$$")
      co[grep("#::.equation.::#",co)[1]] <- paste0(delim,"\n",eq[i],"\n",delim)
    }
    return(co)
  }
  
  collectRealizations <- function(m, cc,modules="../modules/") {
    m <- sub("[0-9]*_","",m)
    if(m=="core") return(NULL)
    rea <- strsplit(cc$modulesInfo[m,"realizations"],",")[[1]]
    folder <- cc$modulesInfo[m,"folder"]
    out <- list()
    for(r in rea) {
      path <- paste0(modules,folder,"/",r,"/equations.gms")
      if(file.exists(path)) out[[r]] <- extractRealization(path)
    }
    module_description <- extractModuleDescription(paste0(modules,folder,"/",folder,".gms"))
    return(list(rdata=out,desc=module_description))
  }
  
  writeModulePage <- function(name,data,module) {
    
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
    
    
    .header(paste0(module$desc$title," (",name,")"),1,zz)
    .header("Description",2,zz)
    .write(module$desc$description,zz)
    
    .header("Interfaces",2,zz)
    .header("Input",3,zz)
    .write(data$input, zz)
    
    .header("Output",3,zz)
    .write(data$output, zz)
    
    .header("Interface plot",3,zz)
    
    .header("Realizations",2,zz)
    
    rdata <- module$rdata
    for(r in names(rdata)) {
      .header(r,3,zz)
      .write(rdata[[r]],zz)
    }
    
    .header("Definitions",2,zz)
    .write(data$declarations, zz)
    
    .header("Developers",2,zz)
    
    .header("See Also",2,zz)
    
    .header("References",2,zz)
    
    close(zz)
  }
   
  out <- collectTables(cc)

  # write doc files
  for(m in setdiff(names(out),"core")) {
    mr <- collectRealizations(m,cc)
    writeModulePage(m,out[[m]],mr)
  }
}