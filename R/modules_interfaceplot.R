#' modules_interfaceplot
#' 
#' Function that applies the \code{\link{interfaceplot}} for a whole model and
#' all its modules.
#' 
#' @param x Either the object returned by \code{\link{codeCheck}} or the path
#' to the main folder of the model.
#' @param modulepath Path to the modules folder
#' @param filetype Filetype that should be used (e.g. "png" or "pdf")
#' @param targetfolder Folder outputs should be written to. If set to NULL outputs will 
#' be added to corresponding module folders
#' @param includeCore Logical to create plot for core or not, default FALSE.
#' @param use_advanced_interfacePlot_function Logical, to choose between interface_plot functions.
#' @param writetable Logical deciding whether a csv containing the interfaces should be written as well.
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
                                  use_advanced_interfacePlot_function = FALSE) {
  if(is.character(x)) x <- codeCheck(x,modulepath)  
  
  if(!is.null(targetfolder)) {
    if(!dir.exists(targetfolder)) dir.create(targetfolder,recursive = TRUE)
  }
  
  if (use_advanced_interfacePlot_function) {
    tmp <- interfaceplot_advanced(x = x,
                                  filename = path(targetfolder,"interfaces"), 
                                  filetype = filetype,
                                  legend = FALSE) 
  } else {
    tmp <- interfaceplot(x,
                         interactive=FALSE,
                         modulepath=modulepath,
                         showInterfaces=FALSE,
                         filename = path(targetfolder,"interfaces"), 
                         filetype = filetype)
  }
  
  
  out <- list()
  
  module_names_with_numbers <- base::list.dirs(path=modulepath,full.names = FALSE,recursive = FALSE)
  if (includeCore) module_names_with_numbers <- c(module_names_with_numbers, "core")
  
  for(d in module_names_with_numbers) {
    target <- ifelse(is.null(targetfolder),path(modulepath,d),targetfolder)
    
    if (use_advanced_interfacePlot_function) {
      mdl <- sub("^[^_]*_","",d)
      tmp <- try(interfaceplot_advanced(x = x,
                                        items_to_display = "(.)m\\S+",
                                        highlight_groups = list(list(name = "highlight",
                                                                     modules = mdl,
                                                                     color = "#ff8f00", # orange
                                                                     shape = "ellipse",
                                                                     links_to_highlight = "outgoing",
                                                                     links_to_ignore = "outside")),
                                        max_length_node_names = 5,
                                        add_nodeName_legend = TRUE,
                                        max_num_edge_labels = "adjust",
                                        max_num_nodes_for_edge_labels = 15,
                                        # !!!! ... inputs:
                                        fade = FALSE,
                                        legend = TRUE,
                                        legend.cex = 0.35,
                                        #repulsion = 0.7, # default = 1
                                        GLratio = 5,
                                        filetype = "png",
                                        filename = paste0(targetfolder,"/interfaces_",mdl))) 
    } else {
      tmp <- try(interfaceplot(x,
                               interactive=FALSE,
                               modulepath=modulepath,
                               modules=sub("^[^_]*_","",d),
                               showInterfaces=TRUE, 
                               filename=path(target,paste("interfaces",sub("^[^_]*_","",d),sep="_")), 
                               filetype=filetype))
    }
    
    if(class(tmp)!="try-error") {
      colnames(tmp) <- c("from","to","no. of interfaces","interfaces")
      out[[d]] <- as.data.frame(tmp,stringsAsFactors = FALSE)
      if(writetable) write.table(tmp,
                                 path(target,paste("interfaces",sub("^[^_]*_","",d),sep="_"),ftype="csv"),
                                 row.names=FALSE,
                                 sep=",",
                                 quote=FALSE,
                                 eol="\r\n")
    }
  }  
  return(out)
}