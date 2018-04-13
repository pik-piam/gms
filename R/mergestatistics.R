#' Merge Statistics
#' 
#' Support function to merge run statistics which have been derived 
#' with \code{\link{runstatistics}}
#' 
#' @param dir Path to the run statistics repository
#' @param file path to an rds-file the data should be written to and from which
#' (if existing) already merged data can be read from
#' @param renew if set to TRUE the full data.table will be created again from scratch,
#' if set to FALSE merging will start with the existing file (if it exists) and just add
#' missing entries 
#' @return A data table containing the merged run statistics
#' @author Jan Philipp Dietrich
#' @importFrom data.table as.data.table rbindlist
#' @importFrom utils type.convert
#' @export

mergestatistics <- function(dir=".", file=NULL, renew=FALSE) {
  out <- NULL
  id  <- NULL
  if(!is.null(file) & !renew) {
    if(file.exists(file)) out <- readRDS(file)
    id <- out$.id
  }
  cwd <- getwd()
  on.exit(setwd(cwd))
  setwd(dir)
  files <- list.files(pattern = "*\\.[rR]da")
  if(length(id)>0) {
    names(files) <- gsub("\\.[rR]da","",files)
    tmp <- setdiff(names(files),id)
    files <- files[tmp]
  }
  if(length(files)==0) return(out)
  stats <- NULL
  outlist <- list()
  for(f in files) {
    load(f)
    if(anyNA(stats$modelstat)) {
      modelstat <- "unknown"
    } else if(all(stats$modelstat<=2)) {
      modelstat <- "optimal"
    } else if(all(stats$modelstat<=2 | stats$modelstat==7)) {
      modelstat <- "non-optimal"
    } else {
      modelstat <- "infeasible"
    }
    stats$solution <- modelstat
    if(is.null(stats$id)) {
      stats$id <- gsub("\\.[rR]da","",f)
    }
    outlist[[stats$id]] <- as.data.table(t(unlist(c(stats[c("user","date","version_management","revision","revision_date","solution")],runtime=as.numeric(stats[["runtime"]], units="hours"),stats$config))))
  }
  out <- rbind(out,rbindlist(outlist, fill=TRUE, idcol=TRUE),fill=TRUE)
  out <- as.data.table(lapply(out, function(x) return(type.convert(as.character(x), as.is=TRUE))))
  names(out) <- make.unique(names(out))
  out <- out[!is.na(out$user),]
  setwd(cwd)
  if(!is.null(file)) saveRDS(out, file=file)
  return(out)
}