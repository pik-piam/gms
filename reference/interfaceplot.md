# interfaceplot

Creates an interface plot of a modular model using
[`qgraph`](https://rdrr.io/pkg/qgraph/man/qgraph.html) and returns the
interface information.

## Usage

``` r
interfaceplot(
  x = ".",
  modules_to_include = NULL,
  modules_to_exclude = NULL,
  links_to_include = NULL,
  links_to_exclude = NULL,
  items_to_include = NULL,
  items_to_exclude = NULL,
  items_to_display = NULL,
  default_groups = list(default1 = list(name = "core", nodes = "core", color = "black",
    shape = "rectangle"), default2 = list(name = "modules", nodes = NULL, color =
    "#6c9ebf", shape = "ellipse")),
  highlight_groups = NULL,
  max_length_node_names = NULL,
  add_nodeName_legend = FALSE,
  max_num_edge_labels = NULL,
  max_num_nodes_for_edge_labels = 30,
  ...
)
```

## Arguments

- x:

  Either an interface list as returned by [`codeCheck`](codeCheck.md) or
  the path to the main folder of the model.

- modules_to_include:

  NULL (default value) or a vector of strings with names of modules to
  include, e.g. c("core", "macro"). If NULL all modules are included.

- modules_to_exclude:

  NULL (default value) or a vector of strings with names of modules to
  exclude, e.g. c("core"). If NULL no modules are excluded.

- links_to_include:

  NULL (default value) or list of lists with attributes "to" and "from",
  that each take a vector of module names, e.g. list(list(to="macro",
  from="core")). If NULL all links are included.

- links_to_exclude:

  NULL (default value) or list of lists with attributes "to" and "from",
  that each take a vector of module names, e.g. list(list(to="macro",
  from="core")). If NULL no links are excluded.

- items_to_include:

  NULL (default value) or a vector of strings with names of items to
  include, e.g. c("vm_cesIO", "pm_pvp"). Regex patterns can also be
  passed, e.g. c("(v\|p)m\_.\*"). If NULL all items are included.

- items_to_exclude:

  NULL (default value) or a vector of strings with names of items to
  exclude, e.g. c("vm_cesIO", "pm_pvp"). Regex patterns can also be
  passed, e.g. c("sm\_.\*"). If NULL no items are excluded.

- items_to_display:

  NULL (default value) or a vector of strings with names of items to
  display, e.g. c("vm_cesIO", "pm_pvp"). Regex patterns can also be
  passed, e.g. c(.m\\ S+). If NULL no items are displayed.

- default_groups:

  List of lists with default group definitions. Defines the default
  formatting of the interface plot. By default, there are two groups,
  see usage, a "core" group made up of only the "core" module and a
  "modules" group made up of all the rest. If a "core" module doesn't
  exist, then that group is simply ignored.

- highlight_groups:

  NULL (default value) or a list of lists with highlight-group
  definitions. By defining highligh groups, additional/specalized
  formatting can be applied to select modules. A group is defined by a
  list with the following attributes:

  - name: a string with the group name. Will appear in legend.

  - nodes: a vector of strings with module names.

  - shape: a string with a valid qgraph shape.

  - color: a string with a valid qgraph color.

  - edges_to_highlight:

    - "all" = edges starting from and ending at the highlight group's
      nodes are colored

    - "incoming" = edges ending at the highlight group's nodes are
      colored

    - "outgoing" = edges starting from the highlight group's nodes are
      colored

    - "within" = only edges that departed from the highlight group's
      nodes and end at them as well are colored

  - edges_to_ignore:

    - "outside" edges that neither start from or end at any of the
      group's nodes are ignored

    - "incoming" edges that do not depart from one of the group's nodes
      are ignored

    - "outgoing" edges that do not arrive at one of the group's nodes
      are ignored

    - "outgoing_to_no_return" edges that departing from nodes outside of
      the group and not ending at nodes within the group are ignored

  An example:
  `list(list(name = "highlight", nodes = "welfare", color = "#ff8f00", shape = "ellipse", edges_to_highlight = "outgoing", edges_to_ignore = "outside"))`.

- max_length_node_names:

  NULL (default value) or an integer n giving the maximum number of
  characters allowed in the node names. If not NULL, node names are
  truncated after n characters, e.g. n=3: "example" -\> "exa.".

- add_nodeName_legend:

  Logical (default FALSE) to add node names in legend, structured by
  group.

- max_num_edge_labels:

  NULL (default value), an integer or the string "adjust". If NULL, all
  edge lables are displayed. If given an integer n, a maximum of n edge
  labels are shown. If set to "adjust", the number of edge labels
  displayed decreases with the number of nodes.

- max_num_nodes_for_edge_labels:

  Integer, (default value = 30). The maximum number of nodes after which
  no edge labels are displayed.

- ...:

  Optional arguments to
  [`qgraph`](https://rdrr.io/pkg/qgraph/man/qgraph.html).

## Value

A tibble with the edge list and interface items.

## Details

What modules (=nodes), links (=edges) and items (=what is passed along
the edges) are taken into account when creating the plot can be
fine-tuned with the "\_include", "\_exclude" arguments.

The "default"- and "highlight_groups" arguments control the formatting
(and also the composition through "highlight_group\$edges_to_ignore").
Groups in qqgraph are a way of clustering nodes together. The default
formatting of the plot is defined with the "default_groups" argument. On
top of that additional groups can be defined with the "highlight_groups"
argument.

The rest of the arguments are pretty self-explanatory. Just remember
that [`qgraph`](https://rdrr.io/pkg/qgraph/man/qgraph.html) arguments
can be passed on as well! Useful ones include: fade=T/F, legend=T/F,
legend.cex (size of the legend font), GLratio (graph/legend size ratio,
edge.label.cex (size of the edge label font)).

## See also

[`codeCheck`](codeCheck.md),[`qgraph`](https://rdrr.io/pkg/qgraph/man/qgraph.html)

## Author

Johannes Koch
