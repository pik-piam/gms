#' fulldataOutput
#' 
#' Creates GAMS code which stores automatically the levels, bounds and
#' marginals of all equations and variables in time depending parameters.
#' 
#' @param declarations_file A GAMS file containing declarations. The function
#' will read declarations from here and add own declarations in an R
#' environment as used by \code{\link{replace_in_file}} (used subject = OUTPUT
#' DECLARATIONS)
#' @param definitions_file A GAMS file which is executed after the solve
#' statement but within the time step loop. Also here code will be added using
#' \code{\link{replace_in_file}} with subject OUTPUT DEFINITIONS
#' @param warn Decides whether a warning should be thrown out, if the
#' declarations file does not exist.
#' @param types Types of outputs that should be written to gdx file. Available
#' types are level, marginal, upper and lower.
#' @param ignore regular expression pattern for variables/equations which
#' should be ignored by fulldataOutput
#' @param loopset Set over which loop runs
#' @author Jan Philipp Dietrich, Felicitas Beier
#' @export
#' @seealso \code{\link{readDeclarations}},\code{\link{replace_in_file}}

fulldataOutput <- function(declarations_file="declarations.gms",definitions_file="postsolve.gms",warn=TRUE,types=c("level","marginal"),ignore="_dummy$",loopset="t") {
  if(!file.exists(declarations_file)) {
    if(warn) warning("Declarations file ",declarations_file," does not exist! No action taken!")
    return(FALSE)
  }
  d <- readDeclarations(declarations_file,unlist=FALSE)
  d <- rbind(d$variable,d$equation)
  if(length(grep(ignore,d[,"names"]))>0) {
    d <- d[-grep(ignore,d[,"names"]),]
    if(dim(d)[1]==0) d <- NULL
  }
  if(!is.null(d)){
    out_names <- paste("o",sub("^([^_]+)m_","\\1_",d[,"names"]),sep="")
    full_out <- NULL
    #if t is already part of the set do not add another t
    for(i in 1:length(out_names)) {
      tmp <- strsplit(d[i,"sets"],split = ",")
      if(length(tmp$sets) > 0 & any(tmp$sets[1] == loopset)) full_out <- c(full_out,sub(",,",",",paste(out_names[i],"(",d[i,"sets"],",type)",sep="")))
      else full_out <- c(full_out,sub(",,",",",paste(out_names[i],"(",loopset,",",d[i,"sets"],",type)",sep="")))
    }
    d <- cbind(d,out_names,full_out)
    charmax <- max(nchar(d[,"full_out"]))
    declarations <- c("parameters",paste("",format(d[,"full_out"],width=charmax),d[,"description"]),";")
    definitions <- NULL
    if("marginal" %in% types) definitions <- c(definitions,sub("()","",paste(" ",sub(",type)",",\"marginal\")",format(d[,"full_out"],width=charmax),fixed=TRUE)," = ",d[,"names"],".m(",d[,"sets"],");",sep=""),fixed=TRUE))
    if("level" %in% types) definitions <- c(definitions,sub("()","",paste(" ",sub(",type)",",\"level\")",format(d[,"full_out"],width=charmax),fixed=TRUE),"    = ",d[,"names"],".l(",d[,"sets"],");",sep=""),fixed=TRUE))
    if("upper" %in% types) definitions <- c(definitions,sub("()","",paste(" ",sub(",type)",",\"upper\")",format(d[,"full_out"],width=charmax),fixed=TRUE),"    = ",d[,"names"],".up(",d[,"sets"],");",sep=""),fixed=TRUE))
    if("lower" %in% types) definitions <- c(definitions,sub("()","",paste(" ",sub(",type)",",\"lower\")",format(d[,"full_out"],width=charmax),fixed=TRUE),"    = ",d[,"names"],".lo(",d[,"sets"],");",sep=""),fixed=TRUE))
    replace_in_file(declarations_file, declarations, subject='OUTPUT DECLARATIONS',add="bottom",addfile=TRUE,comment='*')
    replace_in_file(definitions_file, definitions, subject='OUTPUT DEFINITIONS',add="bottom",addfile=TRUE,comment='*')
  }
  return(TRUE)
}
