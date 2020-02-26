#' interfaceplot_advanced
#'
#' Creates interface plot of modular model and returns interface information.
#'
#' @param x Either an interface list as returned by lucode::codecheck or
#' the path to the main folder of the model.
#' @param modules_to_include Vector of strings with names of modules to include
#' @param modules_to_exclude Vector of strings with names of modules to include
#' @param links_to_include Vector of strings with names of modules to include
#' @param links_to_exclude Vector of strings with names of modules to include
#' @param items_to_include Vector of strings with names of modules to include
#' @param items_to_exclude Vector of strings with names of modules to include
#' @param items_to_display Vector of strings with names of modules to include
#' @param default_groups List of lists with default group definitions
#' @param highlight_groups List of lists with highlight group definitions
#' @param max_length_node_names An integer giving the maximum number of characters allowed in the node
#' names
#' @param add_nodeName_legend Logical to add node names in legend, structured by group
#' @param max_num_edge_labels An integer or the string "adjust"
#' @param max_num_nodes_for_edge_labels Integer. If the number of nodes surpasses it, no edge labels are plotted
#' @param ... Parameters that are passed to the qgraph function
#'
#' @return A matrix with the edge list and interface items.
#' @importFrom dplyr %>% tibble select pull mutate filter if_else bind_rows
#' @importFrom stringr str_extract_all str_remove_all str_trunc
#' @importFrom rlang .data is_empty
#' @author Johannes Koch
#' @export
#'
interfaceplot_advanced <- function(x = ".",
                                   modules_to_include = NULL,
                                   modules_to_exclude = NULL,
                                   links_to_include = NULL,
                                   links_to_exclude = NULL,
                                   items_to_include = NULL,
                                   items_to_exclude = NULL,
                                   items_to_display = NULL, 
                                   default_groups = list(default1 = list(name = "core",
                                                                         color = "black",
                                                                         shape = "rectangle",
                                                                         modules = c("core")),
                                                         default2 = list(name = "modules",
                                                                         color = "#6c9ebf",
                                                                         shape = "ellipse",
                                                                         modules = NULL)),
                                   highlight_groups = NULL,
                                   max_length_node_names = NULL,
                                   add_nodeName_legend = FALSE,
                                   max_num_edge_labels = NULL,
                                   max_num_nodes_for_edge_labels = 30,
                                   ...) {
  
  # STEP 1: Get the interface info of remind -> at the end = an edge list for qqgraph
  ########################################################################################
  # Perform codeCheck if necessary, to get modul interafce info
  if(is.character(x)) x <- codeCheck(x, "modules")  
  
  # Store the interface info in a clear way:
  # 1 column = string with start module
  # 2 column = string with end module
  # 3 column = string with number of interface entites (items) passed from one to the other
  # 4 column = string with all the items seperated with " "
  interface_info <- tibble()
  for(o in names(x)) {
    for(i in setdiff(names(x), o)) {
      items <- intersect(x[[o]][names(x[[o]])=="out"],
                         x[[i]][names(x[[i]])=="in"])
      if (length(items) != "0") {
        interface_info <- bind_rows(interface_info,
                                    tibble("from"=o, 
                                           "to"=i, 
                                           "num_items"=length(items), 
                                           "items"=paste(items, collapse=" ")))
        
      }
    }
  }
  ########################################################################################
  
  
  
  
  # STEP 2: Filter the interface_info 
  ########################################################################################
  # Filter by module
  if(!is.null(modules_to_include)){
    interface_info <- interface_info %>% 
      filter(.data$from %in% modules_to_include | .data$to %in% modules_to_include)
  }
  if(!is.null(modules_to_exclude)){
    interface_info <- interface_info %>% 
      filter(!(.data$from %in% modules_to_exclude | .data$to %in% modules_to_exclude))
  }
  # Filter by link
  if(!is.null(links_to_include)){
    interface_info <- interface_info %>% 
      filter(apply(sapply(links_to_include, function(y) .data$from %in% y$from & .data$to %in% y$to),1,any))
  }
  if(!is.null(links_to_exclude)){
    interface_info <- interface_info %>% 
      filter(apply(sapply(links_to_exclude, function(y) !(.data$from %in% y$from & .data$to %in% y$to)),1,all))
  }
  # Filter by item 
  if(!is.null(items_to_include)){
    my_regex <- paste0(items_to_include, collapse ="|") 
    interface_info$items <- interface_info %>% 
      select(items) %>%
      apply(1, str_extract_all, my_regex, simplify = TRUE) %>%
      sapply(paste, collapse=" ")
    interface_info <- filter(interface_info, items != "")
  } 
  if(!is.null(items_to_exclude)){
    my_regex <- paste0(items_to_exclude, collapse ="|") 
    interface_info <- interface_info %>% 
      mutate(items = str_remove_all(items, pattern = my_regex)) %>%
      filter(items != "")
  }
  
  # Filter in case of "links_to_ignore" is not NULL for any highlight group
  if (any(sapply(highlight_groups, function(y) !is.null(y$links_to_ignore)))) {
    # Set everything to link to be ignored...
    interface_info <- interface_info %>% mutate(link_to_keep = FALSE)
    
    for (hl in highlight_groups) {
      # ... and now add them back. (!Reverse logic!)
      if (!is.null(hl$links_to_ignore)) {
        if (hl$links_to_ignore == "outside") {
          interface_info <- interface_info %>% 
            mutate(link_to_keep = if_else(.data$to %in% hl$modules | .data$from %in% hl$modules, 
                                          TRUE,
                                          .data$link_to_keep))
        }
        if (hl$links_to_ignore == "incoming") {
          interface_info <- interface_info %>% 
            mutate(link_to_keep = if_else(.data$from %in% hl$modules, TRUE, .data$link_to_keep))
        }
        if (hl$links_to_ignore == "outgoing") {
          interface_info <- interface_info %>% 
            mutate(link_to_keep = if_else(.data$to %in% hl$modules, TRUE, .data$link_to_keep))
        }
        if (hl$links_to_ignore == "outgoing_to_noReturn") {
          outside_nodes_that_give_input <- interface_info %>% 
            filter(!.data$from %in% hl$modules & .data$to %in% hl$modules) %>%
            select(.data$from)  %>% unlist() %>% unique()
          
          interface_info <- interface_info %>% 
            mutate(link_to_keep = if_else(.data$to %in% hl$modules | 
                                            (.data$from %in% hl$modules & .data$to %in% outside_nodes_that_give_input), 
                                          TRUE, .data$link_to_keep))
        }
      }
      
    }
    interface_info <- interface_info %>% filter(.data$link_to_keep==TRUE) %>% select(-.data$link_to_keep)
  }
  
  # Check that there is something left
  if(dim(interface_info)[1]==0) {
    warning("Nothing to plot anymore after filters have been applied!")
    return(interface_info)
  }
  if(!requireNamespace("qgraph", quietly = TRUE)) {
    warning("The package qgraph is required for creating interface plots!")
    return(interface_info)
  }
  ########################################################################################
  
  
  
  # STEP 3: Save edge list now, and define some variables for defining the plot_options
  ########################################################################################
  # Save the interface into without items as the weighted edge list to be passed to qgrpah
  edge_list <- select(interface_info, -items)
  
  # Define some usefull variables
  node_names <- interface_info %>% select(.data$from, .data$to) %>% unlist() %>% unique()
  num_nodes <- length(node_names)
  edge_names <- interface_info %>% select(.data$from) %>% unlist()
  num_edges <- length(edge_names)
  default_shape <- "ellipse"
  default_color <- "#6c9ebf"
  ########################################################################################
  
  
  
  # STEP 4: Assign nodes to groups. A node can only be assigned to one group!
  ########################################################################################
  my_groups <- list(name=NULL,
                    color=NULL,
                    shape=NULL,
                    modules=NULL,
                    assignments=NULL)
  
  nodes_left_to_assign <- node_names
  
  # Start with the highlight groups, if they exist.
  if (!is.null(highlight_groups)) {
    for (hlgr in highlight_groups) {
      if (rlang::is_empty(nodes_left_to_assign)) break
      
      my_groups$name <- c(my_groups$name, hlgr$name)
      my_groups$color <- c(my_groups$color, hlgr$color)
      my_groups$shape <- c(my_groups$shape, hlgr$shape)
      
      my_groups$modules[hlgr$name] <- list(nodes_left_to_assign[nodes_left_to_assign%in%hlgr$modules])
      my_groups$assignments[hlgr$name] <- list(which(node_names%in%hlgr$modules))
      nodes_left_to_assign <- nodes_left_to_assign[!nodes_left_to_assign%in%hlgr$modules]
    }
  }

  # Assign modules that are left, two default groups 
  for (dfgr in default_groups) {
    if (rlang::is_empty(nodes_left_to_assign)) break
    
    if (!is.null(dfgr$modules) && any(dfgr$modules %in% nodes_left_to_assign)) {
      my_groups$name <- c(my_groups$name, dfgr$name)
      my_groups$color <- c(my_groups$color, dfgr$color)
      my_groups$shape <- c(my_groups$shape, dfgr$shape)
      
      my_groups$modules[dfgr$name] <- list(nodes_left_to_assign[nodes_left_to_assign%in%dfgr$modules])
      my_groups$assignments[dfgr$name]<- list(which(node_names%in%dfgr$modules))
      
      nodes_left_to_assign <- nodes_left_to_assign[!nodes_left_to_assign%in%dfgr$modules]
    } 
    if (is.null(dfgr$modules)) {
      my_groups$name <- c(my_groups$name, dfgr$name)
      my_groups$color <- c(my_groups$color, dfgr$color)
      my_groups$shape <- c(my_groups$shape, dfgr$shape)
      
      my_groups$modules[dfgr$name] <-list(nodes_left_to_assign)
      my_groups$assignments[dfgr$name] <- list(which(node_names%in%nodes_left_to_assign))
    }
  }
  ########################################################################################
  
  
  
  # STEP 5: Prepare qgraph parameters: content
  ########################################################################################
  # Group assignements
  group_assignments <- my_groups$assignments
  
  # Node shape: one shape per node depending on group
  shape_nodes <- rep(" ", each=num_nodes)
  names(shape_nodes) <- node_names
  for (gr in 1:length(my_groups$name)) {
    shape_nodes[my_groups$modules[[gr]]] <- my_groups$shape[gr]
  }
  
  # Node color: one color per group
  col_nodes <- my_groups$color

  # Edge color: one color per edge
  col_edges <- rep(default_groups$default2$color, each=num_edges)
  names(col_edges) <- edge_names
  
  # Apply edge color highlights
  if (!is.null(highlight_groups)) {
    for (hl in highlight_groups) {
      if (!is.null(hl$links_to_highlight)) {
        # Highlight DEPARTING and INCOMING edges 
        if (hl$links_to_highlight == "all") {
          col_edges[interface_info$from %in% hl$modules | interface_info$to %in% hl$modules] <- hl$color
        }
        # Highlight DEPARTING edges 
        if (hl$links_to_highlight == "outgoing") {
          col_edges[interface_info$from %in% hl$modules] <- hl$color
        }
        # Highlight INCOMING edges.
        if (hl$links_to_highlight == "incoming") {
          col_edges[interface_info$to %in% hl$modules] <- hl$color
        }
        # Highlight INCOMING edges.
        if (hl$links_to_highlight == "within") {
          col_edges[interface_info$from %in% hl$modules & interface_info$to %in% hl$modules] <- hl$color
        }
      }
    }
  }
  
  # If no default_goups$color[2] node is left, use default_groups$color[1] on edges
  # if (all(col_nodes!=default_groups$default2$color)) {
  #   col_edges[col_edges==default_groups$default2$color] <- default_groups$default1$color
  # }
  
  # Edge labels
  if(is.null(items_to_display)) {
    lab_edges <- list()
  } else {
    my_regex <- paste0(items_to_display, collapse ="|") 

    lab_edges <- interface_info %>% 
      pull(items) %>% 
      as.list() %>% 
      lapply(str_extract_all, my_regex, simplify=T) %>%
      lapply(paste, collapse="\n") %>% 
      unlist()
    
    if (all(lab_edges=="")) lab_edges <- list()
  }
  
  
  # Special case when the groups all consits of single modules!!
  if (all(sapply(my_groups$modules, length)==1)) {

    for (i in 1:length(my_groups$assignments)) {
      group_assignments[my_groups$assignments[[i]]] <- my_groups$assignments[i]
    }
    names(group_assignments) <- node_names

    col_nodes <- rep(" ", each=num_nodes)
    names(col_nodes) <- node_names

    for (gr in 1:length(my_groups$name)) {
      col_nodes[my_groups$modules[[gr]]] <- my_groups$color[gr]
    }
    
    if (add_nodeName_legend) {
      cat("Notice: no node-name legend will be added, since there are no more than 3 nodes.\n")
      add_nodeName_legend <- FALSE
    }
  }
  ########################################################################################
  
  
  
  # STEP 6: Prepare qgraph parameters: graphics
  ########################################################################################  
  # Shorten module names to max_length_node_names to fit into plot 
  if (!is.null(max_length_node_names)) {
    n <- max_length_node_names
    
    # Make sure that nodes don't have the same name after cut!
    check_n <- function(y,n) {
      z <- str_trunc(y, n, ellipsis=".")
      if (!any(duplicated(z))) {
        return(n)
      } else {
        while (any(duplicated(z))) {
          n <- n + 1 
          z <- str_trunc(y, n, ellipsis=".")
        }
      }
      return(n)
    }
    n <- check_n(node_names, n)
    
    edge_list <- interface_info %>% 
      mutate(from = stringr::str_trunc(.data$from, n, ellipsis = "."),
             to = stringr::str_trunc(.data$to, n, ellipsis = ".")) %>%
      select(-items)
  
    names(shape_nodes) <- stringr::str_trunc(names(shape_nodes), n, ellipsis=".")
    names(col_edges) <- stringr::str_trunc(names(col_edges), n, ellipsis=".")
  }
  
  # Shorten item lists on edge lables
  if (!rlang::is_empty(lab_edges) && !is.null(max_num_edge_labels)) {
    if (max_num_edge_labels == "adjust") {
      n <- if_else(num_nodes<=3, 15, if_else(num_nodes>8, 1, 9-num_nodes))
    } else {
      n <- max_num_edge_labels
    }

    my_regex1 <- paste0("\\n[^\\n]*", paste0(rep("\\n[^\\n]*", n-1), collapse = ""))
    # Are there edges with too many labels?
    if (any(!is.na(str_extract(lab_edges, my_regex1)))) {
      my_regex2 <- paste0("^.*[^\\n]", paste0(rep("\\n[^\\n]*", n-1), collapse = ""))
      lab_edges <- if_else(!is.na(str_extract(lab_edges, my_regex1)),
                           paste0(str_extract(lab_edges, my_regex2), "\n(...)"),
                           lab_edges) 
      my_title <- "Long lists of interface items are abbreviated with '(...)'"
    } 
  }
  ########################################################################################  
  
  

  # STEP 7: Store all qgraph parameters in the "params" list and create graph
  ######################################################################################## 
  params <- list(...)
  params$input <- edge_list
  params$edgelist <- TRUE
  params$layout <- "spring"
  params$mode <- "strength"
  params$groups <- group_assignments
  params$shape <- shape_nodes
  params$color <- col_nodes
  params$edge.color <- col_edges

  # Add node name legend
  if (add_nodeName_legend) params$nodeNames <- node_names
  
  # Set ratio of a and b alxis of the ellipses, if not already given by ...
  if (!"vsize2" %in% names(params)) params$vsize2 <- 2/3
  
  # Edge labels, only displayed if the graph isn't to full of nodes!
  if (!rlang::is_empty(lab_edges)) {
    if (num_nodes <= max_num_nodes_for_edge_labels) {
      params$edge.labels <- lab_edges
      # Change some default edge.label parameters, if not already given by ...
      if (!"edge.label.position" %in% names(params)) params$edge.label.position <- 0.4
      if (!"edge.label.margin" %in% names(params)) params$edge.label.margin <- 0.1
      if (!"edge.label.cex" %in% names(params)) params$edge.label.cex <- if_else(num_nodes<=9, 
                                                                                 0.65, 
                                                                                 0.65-0.05*(num_nodes-9))
      # Set fade to false if items edge labels are to be displayed, if not already given by ...
      if (!"fade" %in% names(params)) params$fade <- FALSE
    } else {
      my_title <- "Interface items are not shown due to lack of space..."
    }
  } 
  
  if (!"title" %in% names(params) && exists("my_title")) {
    params$title <- my_title
    params$title.cex <- 0.8
  }
  
  qgraphObject <- do.call(qgraph::qgraph, params) 
  ######################################################################################## 

  return(interface_info)
}
