#' interfaceplot
#'
#' Creates an interface plot of a modular model using
#' \code{\link[qgraph]{qgraph}} and returns the interface information.
#'
#' What modules (=nodes), links (=edges) and items (=what is passed along the
#' edges) are taken into account when creating the plot can be fine-tuned with
#' the "_include", "_exclude" arguments.
#'
#' The "default"- and "highlight_groups" arguments control the formatting (and
#' also the composition through "highlight_group$edges_to_ignore"). Groups in
#' qqgraph are a way of clustering nodes together. The default formatting of the
#' plot is defined with the "default_groups" argument. On top of that additional
#' groups can be defined with the "highlight_groups" argument.
#'
#' The rest of the arguments are pretty self-explanatory. Just remember that
#' \code{\link[qgraph]{qgraph}} arguments can be passed on as well! Useful ones
#' include: fade=T/F, legend=T/F, legend.cex (size of the legend font), GLratio
#' (graph/legend size ratio, edge.label.cex (size of the edge label font)).
#'
#' @param x Either an interface list as returned by
#'   \code{\link{codeCheck}} or the path to the main folder of the
#'   model.
#' @param modules_to_include NULL (default value) or a vector of strings with
#'   names of modules to include, e.g. c("core", "macro"). If NULL all modules
#'   are included.
#' @param modules_to_exclude NULL (default value) or a vector of strings with
#'   names of modules to exclude, e.g. c("core"). If NULL no modules are
#'   excluded.
#' @param links_to_include NULL (default value) or list of lists with attributes
#'   "to" and "from", that each take a vector of module names, e.g.
#'   list(list(to="macro", from="core")). If NULL all links are included.
#' @param links_to_exclude NULL (default value) or list of lists with attributes
#'   "to" and "from", that each take a vector of module names, e.g.
#'   list(list(to="macro", from="core")). If NULL no links are excluded.
#' @param items_to_include NULL (default value) or a vector of strings with
#'   names of items to include, e.g. c("vm_cesIO", "pm_pvp"). Regex patterns can
#'   also be passed, e.g. c("(v|p)m_.*"). If NULL all items are included.
#' @param items_to_exclude NULL (default value) or a vector of strings with
#'   names of items to exclude, e.g. c("vm_cesIO", "pm_pvp"). Regex patterns can
#'   also be passed, e.g. c("sm_.*"). If NULL no items are excluded.
#' @param items_to_display NULL (default value) or a vector of strings with
#'   names of items to display, e.g. c("vm_cesIO", "pm_pvp"). Regex patterns can
#'   also be passed, e.g. c(.m\\ S+). If NULL no items are displayed.
#' @param default_groups List of lists with default group definitions. Defines
#'   the default formatting of the interface plot. By default, there are two
#'   groups, see usage, a "core" group made up of only the "core" module and a
#'   "modules" group made up of all the rest. If a "core" module doesn't exist,
#'   then that group is simply ignored.
#' @param highlight_groups NULL (default value) or a list of lists with
#'   highlight-group definitions. By defining highligh groups,
#'   additional/specalized formatting can be applied to select modules. A group
#'   is defined by a list with the following attributes: \itemize{ \item name: a
#'   string with the group name. Will appear in legend. \item nodes: a vector of
#'   strings with module names. \item shape: a string with a valid qgraph shape.
#'   \item color: a string with a valid qgraph color. \item edges_to_highlight:
#'   \itemize{\item NULL = no edges are colored \item "all" = edges starting
#'   from and ending at the highlight group's nodes are colored \item "incoming"
#'   = edges ending at the highlight group's nodes are colored \item "outgoing"
#'   = edges starting from the highlight group's nodes are colored \item
#'   "within" = only edges that departed from the highlight group's nodes and
#'   end at them as well are colored} \item edges_to_ignore: \itemize{\item NULL
#'   = no edges are ignored \item "outside" edges that neither start from or end
#'   at any of the group's nodes are ignored \item "incoming" edges that do not
#'   depart from one of the group's nodes are ignored \item "outgoing" edges
#'   that do not arrive at one of the group's nodes are ignored \item
#'   "outgoing_to_no_return" edges that departing from nodes outside of the
#'   group and not ending at nodes within the group are ignored }} An example:
#'   \code{list(list(name = "highlight", nodes = "welfare", color = "#ff8f00",
#'   shape = "ellipse", edges_to_highlight = "outgoing", edges_to_ignore =
#'   "outside"))}.
#' @param max_length_node_names NULL (default value) or an integer n giving the
#'   maximum number of characters allowed in the node names. If not NULL, node
#'   names are truncated after n characters, e.g. n=3: "example" -> "exa.".
#' @param add_nodeName_legend Logical (default FALSE) to add node names in
#'   legend, structured by group.
#' @param max_num_edge_labels NULL (default value), an integer or the string
#'   "adjust". If NULL, all edge lables are displayed. If given an integer n, a
#'   maximum of n edge labels are shown.  If set to "adjust", the number of edge
#'   labels displayed decreases with the number of nodes.
#' @param max_num_nodes_for_edge_labels Integer, (default value = 30). The
#'   maximum number of nodes after which no edge labels are displayed.
#' @param ... Optional arguments to \code{\link[qgraph]{qgraph}}.
#'
#' @return A tibble with the edge list and interface items.
#'
#' @importFrom dplyr %>% tibble select pull mutate filter if_else bind_rows
#' @importFrom stringr str_extract_all str_remove_all str_trunc
#' @importFrom rlang .data is_empty
#'
#' @seealso \code{\link{codeCheck}},\code{\link[qgraph]{qgraph}}
#'
#' @author Johannes Koch
#' @export
#' 
interfaceplot <- function(x = ".",
                          modules_to_include = NULL,
                          modules_to_exclude = NULL,
                          links_to_include = NULL,
                          links_to_exclude = NULL,
                          items_to_include = NULL,
                          items_to_exclude = NULL,
                          items_to_display = NULL, 
                          default_groups = list(default1 = list(name = "core",
                                                                nodes = "core",
                                                                color = "black",
                                                                shape = "rectangle"),
                                                default2 = list(name = "modules",
                                                                nodes = NULL,
                                                                color = "#6c9ebf",
                                                                shape = "ellipse")),
                          highlight_groups = NULL,
                          max_length_node_names = NULL,
                          add_nodeName_legend = FALSE,
                          max_num_edge_labels = NULL,
                          max_num_nodes_for_edge_labels = 30,
                          ...) {
  
  # STEP 1: Get the interface info of model. At the end, the info will take the shape of an
  # edge list for qqgraph
  ########################################################################################
  # Perform codeCheck if necessary, to get module interface info
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
  
  # Filter in case "highlight_groups$edges_to_ignore" is not NULL for any highlight group
  if (any(sapply(highlight_groups, function(y) !is.null(y$edges_to_ignore)))) {
    # Add column link_to_keep and set to FALSE for all...
    interface_info <- interface_info %>% mutate(link_to_keep = FALSE)
    
    for (hl in highlight_groups) {
      # ... and now set to TRUE the links the links we actually want to keep
      if (!is.null(hl$edges_to_ignore)) {
        if (hl$edges_to_ignore == "outside") {
          interface_info <- interface_info %>% 
            mutate(link_to_keep = if_else(.data$to %in% hl$nodes | .data$from %in% hl$nodes, 
                                          TRUE,
                                          .data$link_to_keep))
        }
        if (hl$edges_to_ignore == "incoming") {
          interface_info <- interface_info %>% 
            mutate(link_to_keep = if_else(.data$from %in% hl$nodes, TRUE, .data$link_to_keep))
        }
        if (hl$edges_to_ignore == "outgoing") {
          interface_info <- interface_info %>% 
            mutate(link_to_keep = if_else(.data$to %in% hl$nodes, TRUE, .data$link_to_keep))
        }
        if (hl$edges_to_ignore == "outgoing_to_noReturn") {
          outside_nodes_that_give_input <- interface_info %>% 
            filter(!.data$from %in% hl$nodes & .data$to %in% hl$nodes) %>%
            select(.data$from)  %>% unlist() %>% unique()
          
          interface_info <- interface_info %>% 
            mutate(link_to_keep = if_else(.data$to %in% hl$nodes | 
                                            (.data$from %in% hl$nodes & .data$to %in% outside_nodes_that_give_input), 
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
      
      my_groups$nodes[hlgr$name] <- list(nodes_left_to_assign[nodes_left_to_assign%in%hlgr$nodes])
      my_groups$assignments[hlgr$name] <- list(which(node_names%in%hlgr$nodes))
      nodes_left_to_assign <- nodes_left_to_assign[!nodes_left_to_assign%in%hlgr$nodes]
    }
  }

  # Assign modules that are left, to the default groups 
  for (dfgr in default_groups) {
    if (rlang::is_empty(nodes_left_to_assign)) break
    
    if (!is.null(dfgr$nodes) && any(dfgr$nodes %in% nodes_left_to_assign)) {
      my_groups$name <- c(my_groups$name, dfgr$name)
      my_groups$color <- c(my_groups$color, dfgr$color)
      my_groups$shape <- c(my_groups$shape, dfgr$shape)
      
      my_groups$nodes[dfgr$name] <- list(nodes_left_to_assign[nodes_left_to_assign%in%dfgr$nodes])
      my_groups$assignments[dfgr$name]<- list(which(node_names%in%dfgr$nodes))
      
      nodes_left_to_assign <- nodes_left_to_assign[!nodes_left_to_assign%in%dfgr$nodes]
    } 
    if (is.null(dfgr$nodes)) {
      my_groups$name <- c(my_groups$name, dfgr$name)
      my_groups$color <- c(my_groups$color, dfgr$color)
      my_groups$shape <- c(my_groups$shape, dfgr$shape)
      
      my_groups$nodes[dfgr$name] <-list(nodes_left_to_assign)
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
    shape_nodes[my_groups$nodes[[gr]]] <- my_groups$shape[gr]
  }
  
  # Node color: one color per group
  col_nodes <- my_groups$color

  # Edge color: one color per edge
  col_edges <- rep(default_groups$default2$color, each=num_edges)
  names(col_edges) <- edge_names
  
  # Apply edge color highlights
  if (!is.null(highlight_groups)) {
    for (hl in highlight_groups) {
      if (!is.null(hl$edges_to_highlight)) {
        # Highlight DEPARTING and INCOMING edges 
        if (hl$edges_to_highlight == "all") {
          col_edges[interface_info$from %in% hl$nodes | interface_info$to %in% hl$nodes] <- hl$color
        }
        # Highlight DEPARTING edges 
        if (hl$edges_to_highlight == "outgoing") {
          col_edges[interface_info$from %in% hl$nodes] <- hl$color
        }
        # Highlight INCOMING edges.
        if (hl$edges_to_highlight == "incoming") {
          col_edges[interface_info$to %in% hl$nodes] <- hl$color
        }
        # Highlight INCOMING edges that departed from modules in highlight group.
        if (hl$edges_to_highlight == "within") {
          col_edges[interface_info$from %in% hl$nodes & interface_info$to %in% hl$nodes] <- hl$color
        }
      }
    }
  }
  
  
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
  if (all(sapply(my_groups$nodes, length)==1)) {

    for (i in 1:length(my_groups$assignments)) {
      group_assignments[my_groups$assignments[[i]]] <- my_groups$assignments[i]
    }
    names(group_assignments) <- node_names

    col_nodes <- rep(" ", each=num_nodes)
    names(col_nodes) <- node_names

    for (gr in 1:length(my_groups$name)) {
      col_nodes[my_groups$nodes[[gr]]] <- my_groups$color[gr]
    }
    
    if (add_nodeName_legend) {
      message("Groups will not be shown in the legend, since they are all composed of a single node.")
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
        message(paste0("Couldn't truncate node names after ", n, " letters (would have resulted in duplicate names)... "))
        while (any(duplicated(z))) {
          n <- n + 1 
          z <- str_trunc(y, n, ellipsis=".")
        }
        message(paste0("truncating after ", n, " letters instead."))
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
      if (!"edge.label.margin" %in% names(params)) params$edge.label.margin <- 0.02
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
