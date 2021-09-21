#' modules_interfaceplot
#'
#' Function that applies \code{\link{interfaceplot}} for a whole model and
#' all its modules.
#'
#' @param x Either the object returned by \code{\link{codeCheck}} or the path to
#'   the main folder of the model.
#' @param modulepath Path to the modules folder
#' @param filetype Filetype that should be used (e.g. "png" or "pdf")
#' @param targetfolder Folder outputs should be written to. If set to NULL
#'   outputs will be added to corresponding module folders
#' @param writetable Logical deciding whether a csv containing the interfaces
#'   should be written as well.
#' @param includeCore Logical to create plot for core or not, default FALSE.
#' @param ... Optional arguments to \code{\link{interfaceplot}}.
#'
#' @return A list with interface tables for each module
#' @author Jan Philipp Dietrich
#' @export
#' @seealso \code{\link{codeCheck}},\code{\link{interfaceplot}}
#' @importFrom utils write.table
modules_interfaceplot <- function(x=".",
                                  modulepath="modules", 
                                  filetype="png", 
                                  targetfolder=NULL, 
                                  writetable=TRUE,
                                  includeCore=FALSE,
                                  ...) {
  if(is.character(x)) x <- codeCheck(x,modulepath)  
  
  if(!is.null(targetfolder)) {
    if(!dir.exists(targetfolder)) dir.create(targetfolder,recursive = TRUE)
  }
  
  if (includeCore) {
    tmp <- interfaceplot(x = x,
                         filename = path(targetfolder,"interfaces"), 
                         filetype = filetype,
                         legend = FALSE,
                         ...) 
  } else {
    tmp <- interfaceplot(x = x,
                         modules_to_exclude = "core",
                         filename = path(targetfolder,"interfaces"), 
                         filetype = filetype,
                         legend = FALSE,
                         ...)
  }
  
  
  out <- list()
  
  module_names_with_numbers <- base::list.dirs(path=modulepath,full.names = FALSE,recursive = FALSE)
  if (includeCore) module_names_with_numbers <- c(module_names_with_numbers, "core")
  
  for(d in module_names_with_numbers) {
    target <- ifelse(is.null(targetfolder),path(modulepath,d),targetfolder)
    
    # mdl = model name without number
    mdl <- sub("^[^_]*_","",d)
    
    # Define interfaceplot parameters
    params <- list(...)
    params$x <- x
    if (!includeCore) params$modules_to_exclude <- "core"
    params$items_to_display <- ".m\\S+"
    params$highlight_groups = list(list(name = "highlight",
                                        nodes = mdl,
                                        color = "#ff8f00", # orange
                                        shape = "ellipse",
                                        edges_to_highlight = "outgoing",
                                        edges_to_ignore = "outside"))
    params$filetype = "png"
    params$filename = paste0(targetfolder,"/interfaces_",mdl)
    # If not already defined in ...
    if (!"add_nodeName_legend" %in% names(params)) params$add_nodeName_legend <- TRUE
    if (!"max_length_node_names" %in% names(params)) params$max_length_node_names <- 5
    if (!"fade" %in% names(params)) params$fade <- FALSE
    if (!"legend.cex" %in% names(params)) params$legend.cex <- 0.35
    if (!"GLratio" %in% names(params)) params$GLratio <- 3
    if (!"edge.label.cex" %in% names(params)) params$edge.label.cex <- 0.65
    
    # Try calling interfaceplot function using params
    tmp <- try(do.call(interfaceplot, params))

    if(!inherits(tmp, "try-error")) {
      colnames(tmp) <- c("from","to","no. of interfaces","interfaces")
      out[[d]] <- as.data.frame(tmp,stringsAsFactors = FALSE)
      if(writetable) write.table(tmp,
                                 path(target,paste("interfaces",mdl,sep="_"),ftype="csv"),
                                 row.names=FALSE,
                                 sep=",",
                                 quote=FALSE,
                                 eol="\r\n")
    }
  }  
  return(out)
}
