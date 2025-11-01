# ==============================================================================
# SECTION: WORKFLOW - ANALYSIS PHASE
# ==============================================================================

#' @title Analyze Model Structure
#' @description This is the first phase of the `lavaanReportR` workflow. The `analyze`
#'   function takes a `lavaan_parameter_table` object and extracts all
#'   essential structural information. It identifies nodes, edges, model features
#'   (e.g., multigroup, multilevel, LGM), and calculates the hierarchical layout.
#' @details
#' The analysis phase is crucial for understanding the model's topology before
#' any styling or plotting decisions are made. It produces a `lavaan_model_structure`
#' object, which serves as the input for the `configure_plot` phase.
#' @seealso \code{\link{configure_plot}}, \code{\link{prepare}}, \code{\link{build}}, \code{\link{render}}
#' @param x An object of class \code{lavaan_parameter_table}.
#' @param ... Additional arguments (not used).
#' @return An object of class \code{lavaan_model_structure}, containing the
#'   analyzed components of the model (nodes, edges, features, layout).
#' @exportS3Method lavaanReportR::analyze
#' @examples
#' \dontrun{
#'   # Assuming 'fit' is a lavaan object
#'   param_table <- lavaan::parameterEstimates(fit)
#'   analyzed_model <- analyze(param_table)
#' }
analyze.lavaan_parameter_table <- function(x, ...) {
  param_table <- x

  # --- 1. Analyze the individual components of the model ---

  # Analyze the overall model features (determines model type, LGM status, etc.)
  features <- .analyze_model_features(param_table)

  # Analyze and extract all potential nodes
  nodes <- .analyze_node_structure(param_table)

  # Analyze and extract all potential edges
  edges <- .analyze_edge_structure(param_table)

  # Analyze and extract the topology of all defined paths
  defined_paths <- .analyze_defined_paths_structure(param_table)

  # Analyze the hierarchical layout BASED ON the extracted nodes and edges
  layout <- .analyze_layout_structure(nodes, edges)

  # --- 2. Assemble the final analysis object ---
  .new_lavaan_model_structure(
    param_table = param_table,
    features = features,
    nodes = nodes,
    edges = edges,
    defined_paths = defined_paths,
    layout = layout
  )
}


# ==============================================================================
# SECTION: WORKFLOW - CONFIGURATION PHASE
# ==============================================================================

#' @title Configure Plot Aesthetics and Settings
#' @description This is the second phase of the `lavaanReportR` workflow. The
#'   `configure_plot` function takes the `lavaan_model_structure` object from the
#'   `analyze` phase and applies all user-defined settings and aesthetic choices.
#'   It merges default recipes with user overrides to create a final, validated
#'   `lavaan_plot_config` object.
#' @details
#' This function acts as the central control panel for customizing the plot's
#' appearance. All arguments are optional and will fall back to sensible defaults.
#' The result is a complete configuration blueprint that guides the `prepare` phase.
#' @seealso \code{\link{analyze}}, \code{\link{prepare}}, \code{\link{build}}, \code{\link{render}}
#' @param x An object of class \code{lavaan_model_structure}.
#' @param estimates_to_show A character string specifying which estimates to use.
#'   Can be `"standardized"`, `"unstandardized"`, or `"none"`.
#' @param recipe A user-defined list to override default recipe settings.
#' @param show_plot_elements A character vector to selectively render elements.
#'   Can contain `"intercepts"`, `"variances"`, `"covariances"`.
#' @param plot_flow_direction A character string to set the plot layout direction.
#'   Can be `"TB"` (top-to-bottom) or `"LR"` (left-to-right).
#' @param show_sig A logical value to show/hide significance stars (e.g., "*").
#' @param show_estimates_above A numeric threshold for displaying estimates.
#'   Values below this are hidden.
#' @param text_size_global A global font size for all plot elements.
#' @param text_size_nodes A font size for all node types.
#' @param text_size_edges A font size for all edge types.
#' @param text_size_details A named list for specific font size overrides (e.g.,
#'   `list(variances = 8)`).
#' @param multilevel_multigroup_order A character vector specifying nesting order
#'   for complex models, must contain `"group"` and `"level"`.
#' @param node_labels A named list for relabeling nodes (e.g., `list(Y1 = "Outcome")`).
#' @param effect_labels A named list for relabeling user-defined path labels.
#' @param ... Not used.
#'
#' @return The final, validated, and complete `lavaan_plot_config` object.
#' @exportS3Method lavaanReportR::configure_plot
#' @examples
#' \dontrun{
#'   # Assuming 'analyzed_model' is from the analyze() step
#'   plot_config <- configure_plot(analyzed_model,
#'                                 plot_flow_direction = "LR",
#'                                 text_size_global = 12)
#' }
configure_plot.lavaan_model_structure <- function(x,
                                                  estimates_to_show = NULL,
                                                  recipe = NULL,
                                                  show_plot_elements = NULL,
                                                  plot_flow_direction = NULL,
                                                  show_sig = NULL,
                                                  show_estimates_above = NULL,
                                                  text_size_global = NULL,
                                                  text_size_nodes = NULL,
                                                  text_size_edges = NULL,
                                                  text_size_details = NULL,
                                                  show_legend = NULL,
                                                  show_legend_with_effects = NULL,
                                                  multilevel_multigroup_order = NULL,
                                                  node_labels = NULL,
                                                  effect_labels = NULL,
                                                  render = NULL,
                                                  ...) {
  analyzed_model <- x

  # --- 1. Collect all user arguments ---
  user_args <- .get_caller_args()
  user_args$x <- NULL # Remove the main object from the list

  # --- 2. Run the configuration pipeline ---
  normalized_args <- .normalize_user_args(user_args)

  final_args <- .apply_user_arg_defaults(normalized_args)

  .validate_user_args(final_args, data_context = analyzed_model$param_table)

  # --- 3. Assemble the final recipe ---
  recipe <- .assemble_recipe(analyzed_model$features, final_args)

  # --- 4. Final Assembly ---
  .new_lavaan_plot_config(
    analyzed_model = analyzed_model,
    user_args = final_args,
    recipe = recipe
  )
}


# ==============================================================================
# SECTION: WORKFLOW - PREPARATION PHASE
# ==============================================================================

#' @title Prepare Final Data for Plotting (from layout)
#' @description This method handles the output of the `layout` phase. It extracts
#'   the configuration object and passes it to the primary `prepare` method.
#' @param x An object of class \code{lavaan_layout}.
#' @param ... Not used.
#' @return A \code{lavaan_graph} object.
#' @keywords internal
#' @exportS3Method lavaanReportR::prepare
#' @noRd
prepare.lavaan_layout <- function(x, ...) {
  # This method serves as a bridge: it unpacks the config object from the
  # lavaan_layout object and passes it to the next method in the chain.
  prepare(x$config)
}


#' @title Prepare Final Data for Plotting
#' @description This is the third phase of the `lavaanReportR` workflow. The
#'   `prepare` function takes the `lavaan_plot_config` object and applies all
#'   styling, labeling, and filtering rules to produce the final, "build-ready"
#'   data tables for nodes and edges.
#' @details
#' This phase is the final data transformation step. It enriches the analyzed
#' model structure with the aesthetic settings from the configuration, resulting
#' in a `lavaan_graph` object that contains everything needed to build the DOT code.
#' @seealso \code{\link{analyze}}, \code{\link{configure_plot}}, \code{\link{build}}, \code{\link{render}}
#' @param x An object of class \code{lavaan_plot_config}.
#' @param ... Not used.
#' @return A \code{lavaan_graph} object containing the final, build-ready
#'   data.tables for nodes and edges, along with the final recipe.
#' @examples
#' \dontrun{
#'   # Assuming 'plot_config' is from the configure_plot() step
#'   prepared_graph <- prepare(plot_config)
#' }
prepare.lavaan_plot_config <- function(x, ...) {
  config <- x

  # --- The Enrichment Pipelines ---
  # We now run two separate, parallel pipelines: one for nodes and one for edges.
  # This aligns with the architectural principle of creating distinct, build-ready
  # data structures for each type of graph element.
  prepared_nodes <- .prepare_nodes_for_build(config)
  prepared_edges <- .prepare_edges_for_build(config)

  # --- Final Assembly of lavaan_graph object ---
  .new_lavaan_graph(
    nodes = prepared_nodes,
    edges = prepared_edges,
    recipe = config$recipe,
    user_args = config$user_args
  )
}


# ==============================================================================
# SECTION: WORKFLOW - BUILD PHASE (FINAL VERSION)
# ==============================================================================

#' @title Build DOT Code for Graphviz
#' @description This is the fourth phase of the `lavaanReportR` workflow. The
#'   `build` function takes the `lavaan_graph` object and constructs the complete
#'   DOT language string required by Graphviz to render the path diagram.
#' @details
#' This function is a "dumb assembler." It translates the prepared data tables
#' into DOT statements without making any further decisions. The output is a
#' `lavaan_dot_code` object, which is a simple wrapper around the final DOT code string.
#' @seealso \code{\link{analyze}}, \code{\link{configure_plot}}, \code{\link{prepare}}, \code{\link{render}}
#' @param x An object of class \code{lavaan_graph}.
#' @param ... Not used.
#' @return A \code{lavaan_dot_code} object containing the final DOT string.
#' @exportS3Method lavaanReportR::build
#' @examples
#' \dontrun{
#'   # Assuming 'prepared_graph' is from the prepare() step
#'   dot_code <- build(prepared_graph)
#' }
build.lavaan_graph <- function(x, ...) {
  graph_obj <- x

  # 1. Erzeuge alle Teile des DOT-Codes durch vektorisierte Aufrufe
  graph_attrs <- .build_graph_statements(graph_obj$recipe)
  node_stmts <- .build_node_statements(graph_obj$nodes)
  rank_stmts <- .build_rank_statements(graph_obj$nodes)
  edge_stmts <- .build_edge_statements(graph_obj$edges)

  # 2. Füge alle Teile zu einem einzigen String zusammen
  full_dot_code <- c(
    graph_attrs,
    "",
    "// 1. Define all nodes (sorted for layout influence)",
    node_stmts,
    "",
    "// 2. Assign nodes to ranks",
    rank_stmts,
    "",
    "// 3. Define all edges",
    edge_stmts,
    "}"
  ) |> paste(collapse = "\n")

  .new_lavaan_dot_code(full_dot_code)
}

# ==============================================================================
# SECTION: WORKFLOW - RENDER PHASE
# ==============================================================================

#' @title Render the Final Plot
#' @description This is the final phase of the `lavaanReportR` workflow. The
#'   `render` function takes the `lavaan_dot_code` object and uses the `DiagrammeR`
#'   package to render the final graph.
#' @details
#' This is the simplest phase of the workflow. It acts as a lightweight wrapper
#' around `DiagrammeR::grViz()`, passing the generated DOT code to be rendered
#' into a final plot object.
#' @seealso \code{\link{analyze}}, \code{\link{configure_plot}}, \code{\link{prepare}}, \code{\link{build}}
#' @param x An object of class \code{lavaan_dot_code}.
#' @param ... Not used.
#' @return A \code{dgr_graph} object from the `DiagrammeR` package.
#' @exportS3Method lavaanReportR::render
#' @examples
#' \dontrun{
#'   # Assuming 'dot_code' is from the build() step
#'   final_plot <- render(dot_code)
#'   # The plot can now be displayed
#'   final_plot
#' }
render.lavaan_dot_code <- function(x, ...) {
  # Die "dümmste" Phase: Nur noch den String an die Render-Engine übergeben.
  DiagrammeR::grViz(x$dot_code)
}
