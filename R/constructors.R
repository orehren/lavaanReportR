#' @title Internal S3 constructor for a `lavaan_parameter_table` object.
#' @description A simple wrapper to assign a class to the parameter table,
#'   enabling S3 dispatch.
#' @param ... A named list, expected to contain the `param_table`.
#' @return An object of class `lavaan_parameter_table`.
#' @keywords internal
.new_lavaan_parameter_table <- function(param_table) {
  structure(param_table, class = c("lavaan_parameter_table", "data.table", "data.frame"))
}


#' @title Internal S3 constructor for a `lavaan_model_structure` object.
#' @description Creates the container for the results of the `analyze` phase.
#' @param ... A named list of analysis results (e.g., `features`, `layout`).
#' @return An object of class `lavaan_model_structure`.
#' @keywords internal
.new_lavaan_model_structure <- function(...) {
  structure(list(...), class = "lavaan_model_structure")
}


#' @title Internal S3 constructor for a `plot_config` object.
#' @description Creates the final configuration object that holds all necessary
#'   components for the subsequent analysis, build, and render phases.
#' @param ... A named list of the final, processed configuration components.
#' @return An object of class `plot_config`.
#' @keywords internal
#' @noRd
.new_lavaan_plot_config <- function(...) {
  structure(list(...), class = c("lavaan_plot_config", "list"))
}


<<<<<<< HEAD
=======
<<<<<<< HEAD
#' @title Constructor for the `lavaan_layout` Class
#' @description Creates a `lavaan_layout` object by adding the class name to an
#'   existing object (typically a `lavaan_graph` object that has been enriched
#'   with layout data).
#' @param x The `lavaan_graph` object to be promoted.
#' @return A `lavaan_layout` object.
#' @keywords internal
#' @noRd
lavaan_layout <- function(x) {
  class(x) <- c("lavaan_layout", class(x))
  x
=======
>>>>>>> restore
#' @title Internal S3 constructor for a `lavaan_layout` object.
#' @description Creates the container for the results of the `layout` phase.
#' @param ... A named list of layout results.
#' @return An object of class `lavaan_layout`.
#' @keywords internal
.new_lavaan_layout <- function(...) {
  structure(list(...), class = "lavaan_layout")
<<<<<<< HEAD
=======
>>>>>>> a663b19 (Revert "feat(analysis, build): Clean up dot attributes and fix defined path IDs")
>>>>>>> restore
}


###################################################


#' @title Internal S3 constructor for a `lavaan_node` object.
#' @description Creates a single node object for the graph. This is a simple
#'   list with a class attribute, containing all final style and label information.
#' @param ... A named list of node attributes (e.g., id, label, shape).
#' @return An object of class `sem_node`.
#' @keywords internal
#' @noRd
.new_lavaan_node <- function(...) {
  structure(list(...), class = "lavaan_node")
}

#' @title Internal S3 constructor for a `lavaan_edge` object.
#' @description Creates a single edge object for the graph.
#' @param ... A named list of edge attributes (e.g., from, to, label, style).
#' @return An object of class `lavaan_edge`.
#' @keywords internal
#' @noRd
.new_lavaan_edge <- function(...) {
  structure(list(...), class = "lavaan_edge")
}

#' @title Internal S3 constructor for a `lavaan_graph`.
#' @description The container for the results of the `build` phase. It holds
#'   the final lists of `lavaan_node` and `lavaan_edge` objects.
#' @param ... A named list of components (nodes, edges, etc.).
#' @return An object of class `lavaan_graph`.
#' @keywords internal
#' @noRd
.new_lavaan_graph <- function(...) {
  structure(list(...), class = "lavaan_graph")
}


#' @title Internal S3 constructor for a `lavaan_dot_code` object.
#' @description A simple container for the final, complete DOT code string,
#'   enabling S3 dispatch for the `render` phase.
#' @param dot_code A character string containing the DOT code.
#' @return An object of class `lavaan_dot_code`.
#' @keywords internal
#' @noRd
.new_lavaan_dot_code <- function(dot_code) {
  structure(list(dot_code = dot_code), class = "lavaan_dot_code")
}
