# ==============================================================================
# SECTION: BUILD & RENDER HELPERS
# These functions are the core of the "dumb assembler" build phase. They take
# the prepared data tables and instantiate the final S3 objects.
# ==============================================================================

#' @title Build a dynamic attribute string from a data.table (Final Idiomatic Version)
#' @description A generic, idiomatic data.table function that takes a wide
#'   data.table and converts its attribute columns into a single, collapsed
#'   "key=value" string for each element. This version uses a clean, vectorized
#'   melt-enrich-transform-aggregate pattern.
#' @param dt The data.table to process.
#' @param by_vars A character vector of columns that uniquely identify an element.
#' @param meta_cols A character vector of columns to ignore during attribute assembly.
#' @return A data.table containing the `by_vars` and a new `attr_string` column.
#' @keywords internal
#' @noRd
.build_dynamic_attributes <- function(dt, by_vars, meta_cols) {
  # 1. Definiere die Attribut-Spalten innerhalb der Funktion. Sauber und gekapselt.
  attribute_cols <- setdiff(names(dt), c(by_vars, meta_cols))

  # 2. Erstelle eine Typen-Lookup-Tabelle für die Attribute.
  col_types <- sapply(dt[, ..attribute_cols], class)
  type_lookup <- data.table::data.table(attribute = names(col_types), type = col_types)

  # 3. Melt: Ein einziger, schneller Vorgang. Akzeptiere die Typ-Koersion.
  suppressWarnings(
    long_dt <- data.table::melt(dt,
      id.vars = by_vars,
      measure.vars = attribute_cols,
      variable.name = "attribute",
      value.name = "value",
      na.rm = TRUE
    )
  )

  # 4. Enrich: Füge die korrekte Typ-Information wieder an.
  long_dt[type_lookup, on = "attribute", type := i.type]

  # 5. Transform: Formatiere die Attribute in einem einzigen, vektorisierten fcase-Aufruf.
  long_dt[, formatted_attr := data.table::fcase(
    type == "logical",   sprintf("%s=%s", attribute, tolower(value)),
    type == "character", sprintf("%s='%s'", attribute, gsub("\\", "\\\\", value, fixed = TRUE)),
    type == "numeric",   sprintf("%s=%s", attribute, value),
    type == "integer",   sprintf("%s=%s", attribute, value)
  )]

  # 6. Aggregate: Fasse die formatierten Attribute pro Element zusammen.
  aggregated_attrs <- long_dt[, .(
    attr_string = paste(formatted_attr, collapse = ", ")
  ), by = by_vars]

  return(aggregated_attrs)
}


#' @title Build DOT statements for nodes (Final Refactored Version)
#' @description A slim wrapper that defines node-specific parameters and calls
#'   the generic attribute builder.
#' @param nodes_dt The final nodes `data.table`.
#' @return A character vector of DOT node definitions.
#' @keywords internal
#' @noRd
.build_node_statements <- function(nodes_dt) {
  if (nrow(nodes_dt) == 0) {
    return(character(0))
  }

  # 1. Definiere die Node-spezifische Konfiguration.
  by_vars <- "id"
  meta_cols <- c(
    "node_unit", "node_type", "group", "level", "sig", "est.std",
    "est.unstd", "rank", "element_unit", "element_unit_order"
  )

  # 2. Rufe den generischen Helper auf.
  aggregated_attrs <- .build_dynamic_attributes(nodes_dt, by_vars, meta_cols)

  # 3. Führe den finalen, Node-spezifischen Assembly-Schritt durch.
  final_dt <- aggregated_attrs[nodes_dt, on = "id"]
  return(final_dt[, sprintf("%s [%s];", id, attr_string)])
}


#' @title Build DOT statements for edges (Final Refactored Version)
#' @description A slim wrapper that defines edge-specific parameters and calls
#'   the generic attribute builder.
#' @param edges_dt The final edges `data.table`.
#' @return A character vector of DOT edge definitions.
#' @keywords internal
#' @noRd
.build_edge_statements <- function(edges_dt) {
  if (nrow(edges_dt) == 0) {
    return(character(0))
  }

  # 1. Definiere die Edge-spezifische Konfiguration.
  by_vars <- c("from", "to", "edge_type")
  meta_cols <- c(
    "id_prefix", "edge_type", "group", "level", "sig", "rhs",
    "est.std", "est.unstd", "mediators", "base_paths"
  )

  # 2. Rufe den generischen Helper auf.
  aggregated_attrs <- .build_dynamic_attributes(edges_dt, by_vars, meta_cols)

  # 3. Führe den finalen, Edge-spezifischen Assembly-Schritt durch.
  final_dt <- aggregated_attrs[edges_dt, on = by_vars] # c("from", "to", "edge_type")
  return(final_dt[, sprintf("%s -> %s [%s];", from, to, attr_string)])
}

#' @title Build global graph attribute statements
#' @description Creates the initial DOT statements for global graph attributes
#'   like layout direction (`rankdir`).
#' @param recipe The recipe from the `lavaan_graph` object.
#' @return A character vector of global DOT statements.
#' @keywords internal
#' @noRd
.build_graph_statements <- function(recipe) {
  c(
    "digraph SEM {",
    "graph [layout=neato, rankdir=%s, splines=spline];" |> sprintf(recipe$rankdir),
    "node [fontname='Helvetica', fontsize = 8, width = 0.3, height = 0.3, fixedsize = TRUE];",
    "edge [fontname='Helvetica', fontsize=11, style = 'solid', arrowhead = 'normal', penwidth = 1.5, arrowsize=0.5];"
  )
}
