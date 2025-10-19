# ==============================================================================
# SECTION: WORKFLOW - ANALYSIS PHASE
# ==============================================================================

#' @describeIn analyze Method for `lavaan_parameter_table` objects.
#' @description This method performs the "analysis" phase of the workflow. It
#'   calls specialized analysis functions to extract structured information about
#'   the model's nodes, edges, defined paths, and overall features.
#'
#' @param x An object of class `lavaan_parameter_table`.
#' @param ... Additional arguments (not used).
#'
#' @return An object of class `lavaan_model_structure`.
#' @exportS3Method lavaanReportR::analyze
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

#' @describeIn configure_plot Method for `lavaan_model_structure` objects.
#' @description This method orchestrates the entire process of creating the
#'   final `lavaan_plot_config` object. It takes the analysis results from the
#'   `analyze` phase and combines them with all user-configurable arguments to
#'   produce the final, validated blueprint for plotting.
#'
#' @param x An object of class `lavaan_model_structure`.
#' @param estimates_to_show A character string specifying which estimates to use.
#'   Can be `"standardized"`, `"unstandardized"`, or `"none"`.
#' @param recipe A user-defined list to override default recipe settings.
#' @param show_plot_elements A character vector to selectively render elements.
#'   Can contain `"intercepts"`, `"variances"`, `"covariances"`.
#' @param plot_flow_direction A character string to set the plot layout direction.
#'   Can be `"TB"` or `"LR"`.
#' @param show_sig A logical value to show/hide significance stars.
#' @param show_estimates_above A numeric threshold for displaying estimates.
#' @param text_size_global A global font size for all plot elements.
#' @param text_size_nodes A font size for all node types.
#' @param text_size_edges A font size for all edge types.
#' @param text_size_details A named list for specific font size overrides.
#' @param show_legend A logical value to show/hide the legend.
#' @param show_legend_with_effects A logical value to include derived effects in the legend.
#' @param multilevel_multigroup_order A character vector specifying nesting order,
#'   must contain `"group"` and `"level"`.
#' @param node_labels A named list for relabeling nodes.
#' @param effect_labels A named list for relabeling user-defined path labels.
#' @param render A logical value. If `FALSE`, the final call to `render()` will
#'   return DOT code instead of a plot.
#' @param ... Not used.
#'
#' @return The final, validated, and complete `lavaan_plot_config` object.
#' @exportS3Method lavaanReportR::configure_plot
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

#' @describeIn prepare Method for `lavaan_plot_config` objects.
#' @description This method takes the final configuration and orchestrates the
#'   full enrichment of the parameter table, creating the final data blueprint
#'   required by the build phase. This is the "prepare" phase of the main workflow.
#' @param x An object of class `lavaan_plot_config`.
#' @param ... Not used.
#' @return A `lavaan_prepared_data` object containing the enriched parameter table
#'   and other necessary components for the build phase.
#' @exportS3Method lavaanReportR::prepare
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

#' @describeIn build Method for `lavaan_graph` objects.
#' @description Takes the final `lavaan_graph` object and assembles the complete
#'   DOT code string using vectorized operations.
#' @param x An object of class `lavaan_graph`.
#' @param ... Not used.
#' @return A `lavaan_dot_code` object containing the final DOT string.
#' @exportS3Method lavaanReportR::build
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

#' @describeIn render Method for `lavaan_dot_code` objects.
#' @description Takes the final DOT code and renders it into a graph object
#'   using the `DiagrammeR` package.
#' @param x An object of class `lavaan_dot_code`.
#' @param ... Not used.
#' @return A `dgr_graph` object from `DiagrammeR`.
#' @exportS3Method lavaanReportR::render
render.lavaan_dot_code <- function(x, ...) {
  # Die "dümmste" Phase: Nur noch den String an die Render-Engine übergeben.
  DiagrammeR::grViz(x$dot_code)
}
