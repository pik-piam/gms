#' interfaceplot
#' 
#' Plots the interface network between the different modules of the model
#' 
#' @param x Either an interface list as returned by \code{\link{codeCheck}} or
#' the path of the main folder of the model
#' @param modulepath path to the module folder relative to "path" (only
#' required if x is the model path)
#' @param cutoff A cutoff determining the minimum number of values that have to
#' transferred between 2 modules so that the link is shown in the plot.
#' @param interactive Decides whether the plot should be interactive or static.
#' If argument is set to NULL the output of function interactive() is used
#' instead.
#' @param modules A subset of modules for which connections should be shown. If
#' NULL all modules are shown
#' @param exclude_modules A subset of modules which should be excluded from the
#' plot. If NULL all modules are shown
#' @param interfaces A subset of interfaces which should be shown (together
#' with all interfaces on the same links). If NULL all interfaces are shown.
#' @param showInterfaces Boolean deciding whether the names of the interfaces
#' should be plotted as labels of the edges or not (plot becomes hard to read
#' if too many labels are plotted).
#' @param filetype File type if plot should be written to a file (e.g. png)
#' @param filename File name without file type
#' @param package package to use for interactive visualization. Available are currently visNetwork and igraph
#' @return A matrix containing additional informations about the links between
#' the modules
#' @author Jan Philipp Dietrich
#' @importFrom grDevices pdf dev.off
#' @export
#' @seealso \code{\link{codeCheck}}
interfaceplot <- function(x=".",modulepath="modules",cutoff=0,interactive=NULL,modules=NULL,exclude_modules=NULL,interfaces=NULL,showInterfaces=TRUE, filetype=NULL, filename=NULL, package="visNetwork") {
  if(is.character(x)) x <- codeCheck(x,modulepath)  
  n <- names(x)
  out <- NULL
  for(o in n) {
    for(i in setdiff(n,o)) {
      links <- intersect(x[[o]][names(x[[o]])=="out"],x[[i]][names(x[[i]])=="in"])
      out <- rbind(out,c(o
                         ,i
                         ,length(links)
                         ,paste(links,collapse=" ")))
    }
  }
  #unknown <- data.frame(row.names=c("SOM"),color=c("#ff0000"))
  unknown <- NULL
  out <- out[as.integer(out[,3])>cutoff,,drop=FALSE]
  if(!is.null(interfaces)) out <- out[grep(paste0(interfaces,collapse ="|"),out[,4]),,drop=FALSE]
  if(!is.null(modules)) out <- out[out[,1]%in%modules | out[,2]%in%modules,,drop=FALSE]
  if(!is.null(exclude_modules)) out <- out[!(out[,1]%in%exclude_modules | out[,2]%in%exclude_modules),,drop=FALSE]
  if(dim(out)[1]==0) stop("Nothing to plot anymore after filters have been applied!")
  if(package=="visNetwork") {

  }   
  if(is.null(interactive)) interactive <- interactive()
  if(interactive) { 
    if(package=="visNetwork") {
      if(!requireNamespace("visNetwork", quietly = TRUE)) stop("The package visNetwork is required for creating visNetwork interface plots!")
      edges <- as.data.frame(out)
      names(edges) <- c("from","to","value","title")
      edges <- edges[edges$value!=0,]
      nodes <- data.frame(id=unique(c(as.character(edges$from),as.character(edges$to))))
      nodes$color <- mip::plotstyle(nodes$id)
      nodes$label <- nodes$id
      edges$color <- mip::plotstyle(edges$from)
      return(visNetwork::visNetwork(nodes,edges))
    } else {
      g <- igraph::graph.edgelist(as.matrix(out[,-(3:4),drop=FALSE]))
      igraph::E(g)$width <- as.integer(out[,3])
      igraph::E(g)$curved <- 0.2
      if(showInterfaces) igraph::E(g)$label <- gsub(" ","\n",out[,4])
    
      #set all label colors to black except for core which should be set to white
      igraph::V(g)$label.color <- rep("black",length(igraph::V(g)$name))
      igraph::V(g)$label.color[igraph::V(g)$name=="core"] <- "white"
      
      #set colors vertices
      igraph::V(g)$color <- mip::plotstyle(igraph::V(g)$name, unknown=unknown)
      
      #set colors edges
      igraph::E(g)$color       <- mip::plotstyle(out[,1], unknown=unknown)    
      igraph::E(g)$label.color <- mip::plotstyle(out[,1], unknown=unknown)  
    
      id <- igraph::tkplot(g)
      tcltk::tkconfigure(igraph::tkplot.canvas(id), "bg"="white")
    }
  } else {
    if(!requireNamespace("qgraph", quietly = TRUE)) warning("The package qgraph is required for creating interface plots!")
    if(showInterfaces) {
      labels <- gsub(" ","\n",out[,4])
    } else {
      labels <- FALSE
    }
    if(!is.null(filetype)) pdf()
    col <- mip::plotstyle(unique(as.vector(out[,1:2])), unknown=unknown)
    col["core"] <- NA
    tmp <- suppressWarnings(try(qgraph::qgraph(out[,1:3, drop=FALSE],
                      edge.color=mip::plotstyle(out[,1], unknown=unknown),
                      edge.labels=labels,
                      color=col,
                      shape="ellipse",
                      vsize2=0.5,
                      edge.label.cex=(1/as.integer(out[,3]))^0.3,
                      filename=filename,
                      filetype=filetype),silent=TRUE))
    if(class(tmp)=="try-error") {
      tmp <- suppressWarnings(try(qgraph::qgraph(out[,1:2],
                        edge.color=mip::plotstyle(out[,1], unknown=unknown),
                        edge.labels=labels,
                        edge.width=as.integer(out[,3]),
                        color=col,
                        shape="ellipse",
                        vsize2=0.5,
                        edge.label.cex=(1/as.integer(out[,3]))^0.3,
                        filename=filename,
                        filetype=filetype),silent=TRUE))
    }
    if(class(tmp)=="try-error") {
      tmp <- suppressWarnings(try(qgraph::qgraph(out[,1:2],
                        edge.color=mip::plotstyle(out[,1], unknown=unknown),
                        edge.labels=labels,
                        color=col,
                        shape="ellipse",
                        vsize2=0.5,
                        edge.label.cex=(1/as.integer(out[,3]))^0.3,
                        filename=filename,
                        filetype=filetype),silent=TRUE))
    }
    if(!is.null(filetype)) dev.off()
  }
  return(out)
}
