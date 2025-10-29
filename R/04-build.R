# ==============================================================================
# SECTION: BUILD PHASE
# ==============================================================================
# This section contains functions for the "build" phase of the workflow. The
# functions in this section are "dumb assemblers." Their sole responsibility is
# to take the prepared data tables from the previous phase and translate them
# into DOT language strings for Graphviz. They do not make any decisions about
# styling or layout.
# ==============================================================================

#' @title Build a dynamic attribute string from a data.table
#' @description A generic function that converts the attribute columns of a
#'   data.table into a single, collapsed "key=value" string for each element.
#'
#' @param dt The data.table to process.
#' @param by_vars A character vector of columns that uniquely identify an element.
#' @param meta_cols A character vector of columns to ignore during attribute assembly.
#'
#' @return A data.table containing the `by_vars` and a new `attr_string` column.
#' @keywords internal
#' @noRd
.build_dynamic_attributes <- function(dt, by_vars, meta_cols) {
  # 1. Identify the attribute columns.
  attribute_cols <- setdiff(names(dt), c(by_vars, meta_cols))

  # 2. Create a lookup table for the data type of each attribute.
  col_types <- sapply(dt[, ..attribute_cols], class)
  type_lookup <- data.table::data.table(attribute = names(col_types), type = col_types)

  # 3. Melt the data to a long format.
  suppressWarnings(
    long_dt <- data.table::melt(dt,
      id.vars = by_vars,
      measure.vars = attribute_cols,
      variable.name = "attribute",
      value.name = "value",
      na.rm = TRUE
    )
  )

  # 4. Add the data type information back to the long-format table.
  long_dt[type_lookup, on = "attribute", type := i.type]

  # 5. Format the attributes into "key=value" strings, quoting character values.
  long_dt[, formatted_attr := data.table::fcase(
    type == "logical",   sprintf("%s=%s", attribute, tolower(value)),
    type == "character", sprintf("%s='%s'", attribute, gsub("\\", "\\\\", value, fixed = TRUE)),
    type == "numeric",   sprintf("%s=%s", attribute, value),
    type == "integer",   sprintf("%s=%s", attribute, value)
  )]

  # 6. Aggregate the formatted attributes into a single string for each element.
  aggregated_attrs <- long_dt[, .(
    attr_string = paste(formatted_attr, collapse = ", ")
  ), by = by_vars]

  return(aggregated_attrs)
}


#' @title Build DOT statements for nodes
#' @description A wrapper that defines node-specific parameters and calls the
#'   generic attribute builder.
#'
#' @param nodes_dt The final nodes `data.table`.
#' @return A character vector of DOT node definitions.
#' @keywords internal
#' @noRd
.build_node_statements <- function(nodes_dt) {
  if (nrow(nodes_dt) == 0) {
    return(character(0))
  }

  # 1. Define the node-specific configuration.
  by_vars <- "id"
  meta_cols <- c(
    "node_unit", "node_type", "group", "level", "sig", "est.std",
    "est.unstd", "rank", "element_unit", "element_unit_order"
  )

  # 2. Call the generic attribute builder.
  aggregated_attrs <- .build_dynamic_attributes(nodes_dt, by_vars, meta_cols)

  # 3. Assemble the final DOT statements.
  final_dt <- aggregated_attrs[nodes_dt, on = "id"]
  return(final_dt[, sprintf("%s [%s];", id, attr_string)])
}


#' @title Build DOT statements for edges
#' @description A wrapper that defines edge-specific parameters and calls the
#'   generic attribute builder.
#'
#' @param edges_dt The final edges `data.table`.
#' @return A character vector of DOT edge definitions.
#' @keywords internal
#' @noRd
.build_edge_statements <- function(edges_dt) {
  if (nrow(edges_dt) == 0) {
    return(character(0))
  }

  # 1. Define the edge-specific configuration.
  by_vars <- c("from", "to", "edge_type")
  meta_cols <- c(
    "id_prefix", "edge_type", "group", "level", "sig",
    "est.std", "est.unstd", "mediators", "base_paths"
  )

  # 2. Call the generic attribute builder.
  aggregated_attrs <- .build_dynamic_attributes(edges_dt, by_vars, meta_cols)

  # 3. Assemble the final DOT statements.
  final_dt <- aggregated_attrs[edges_dt, on = by_vars]
  return(final_dt[, sprintf("%s -> %s [%s];", from, to, attr_string)])
}

#' @title Build DOT statements for rank assignments
#' @description Groups nodes by their rank and creates `{rank=same; ...}`
#'   statements to enforce the hierarchical layout in Graphviz.
#'
#' @param nodes_dt The final nodes `data.table`, which must contain a `rank` column.
#' @return A character vector of DOT rank statements.
#' @keywords internal
#' @noRd
.build_rank_statements <- function(nodes_dt) {
  if (!"rank" %in% names(nodes_dt) || nrow(nodes_dt) == 0) {
    return(character(0))
  }

  # 1. Group the node IDs by their rank.
  rank_groups <- nodes_dt[, .(
    nodes_str = paste(id, collapse = "; ")
  ), by = rank]

  # 2. Create the `{rank=same; ...}` statements.
  rank_groups[, sprintf("{ rank=same; %s };", nodes_str)]
}


#' @title Build global graph attribute statements
#' @description Creates the initial DOT statements for global graph attributes
#'   like layout direction (`rankdir`).
#'
#' @param recipe The recipe from the `lavaan_graph` object.
#' @return A character vector of global DOT statements.
#' @keywords internal
#' @noRd
.build_graph_statements <- function(recipe) {
  # These are the global settings for the graph, including default styles for
  # all nodes and edges.
  c(
    "digraph SEM {",
    "graph [layout=dot, rankdir=%s, splines=spline, nodesep=0.1, ranksep=0.5];" |> sprintf(recipe$rankdir),
    "node [fontname='Helvetica', fontsize = 8, width = 0.3, height = 0.3, fixedsize = TRUE];",
    "edge [fontname='Helvetica', fontsize=11, style = 'solid', arrowhead = 'normal', penwidth = 1.5, arrowsize=0.5];"
  )
}
