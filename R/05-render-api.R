# ==============================================================================
# SECTION: WORKFLOW - ANALYSIS PHASE
# ==============================================================================

#' @title 1. Analyze Model Structure
#' @description This is the first phase of the `lavaanReportR` workflow. The `analyze`
#'   function ingests a `lavaan` parameter table and performs a deep analysis of
#'   the model's structure. It identifies all nodes (latent and manifest variables),
#'   edges (regressions, loadings, etc.), and key model features (e.g., multigroup,
#'   multilevel, LGM), and calculates the hierarchical layout.
#' @details
#' The analysis phase is crucial for understanding the model's topology before
#' any styling or plotting decisions are made. It produces a `lavaan_model_structure`
#' object, a blueprint of the model that serves as the input for the `configure_plot` phase.
#' @param x A `data.frame` or `data.table` containing the model parameters,
#'   typically the output of `lavaan::parameterEstimates(fit)`.
#' @param ... Additional arguments (currently not used).
#' @return An object of class `lavaan_model_structure`, which contains the
#'   analyzed components of the model (nodes, edges, features, layout). This
#'   object is the input for the next step in the workflow, `configure_plot()`.
#' @seealso \code{\link{configure_plot}}, \code{\link{prepare}}, \code{\link{build}}, \code{\link{render}}
#' @export
#' @examples
#' \dontrun{
#' library(lavaan)
#'
#' # 1. Fit a lavaan model
#' model <- '
#'   # measurement model
#'     ind60 =~ x1 + x2 + x3
#'     dem60 =~ y1 + y2 + y3 + y4
#'   # structural model
#'     dem60 ~ ind60
#' '
#' fit <- sem(model, data = PoliticalDemocracy)
#'
#' # 2. Get the parameter table
#' param_table <- lavaan::parameterEstimates(fit)
#'
#' # 3. Analyze the model structure
#' analyzed_model <- analyze(param_table)
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

#' @title 2. Configure Plot Aesthetics and Settings
#' @description This is the second phase of the `lavaanReportR` workflow. It
#'   takes the analyzed model structure and applies all user-defined settings
#'   and aesthetic choices. It serves as the main control panel for customizing
#'   the plot's appearance.
#' @details
#' All arguments are optional and will fall back to sensible defaults defined
#' in the package's "recipes". The function merges default recipes with any
#' user overrides to create a final, validated `lavaan_plot_config` object, which
#' then guides the `prepare` phase.
#' @param x An object of class `lavaan_model_structure` from the `analyze()` phase.
#' @param estimates_to_show A character string specifying which estimates to display
#'   on the plot. One of `"standardized"` (default) or `"unstandardized"`.
#' @param recipe A named list of settings to override the default recipe. This
#'   is for advanced users who want to define a completely custom set of styles.
#' @param show_plot_elements A character vector to selectively render specific
#'   structural elements. Can contain any combination of `"intercepts"`,
#'   `"variances"`, and `"covariances"`. By default, all are shown.
#' @param plot_flow_direction A character string setting the primary layout
#'   direction. One of `"TB"` (top-to-bottom, default) or `"LR"` (left-to-right).
#' @param show_sig A logical value indicating whether to show significance stars
#'   (e.g., "*", "**") next to the estimates. Default is `TRUE`.
#' @param show_estimates_above A numeric threshold for displaying path estimates.
#'   Estimates with absolute values below this threshold will be hidden. Default is `0`.
#' @param text_size_global A single numeric value for the global font size of all
#'   plot elements. Specific overrides will take precedence.
#' @param text_size_nodes A numeric value for the font size of all nodes.
#' @param text_size_edges A numeric value for the font size of all edges.
#' @param text_size_details A named list for fine-grained font size overrides.
#'   For example, `list(variances = 8, intercepts = 8)`.
#' @param multilevel_multigroup_order For complex models, a character vector
#'   specifying the nesting order. Must contain `"group"` and `"level"`.
#' @param node_labels A named list for relabeling nodes. For example,
#'   `list(ind60 = "Industrialization", dem60 = "Democracy 60")`.
#' @param effect_labels A named list for relabeling user-defined (`:=`) path labels.
#' @param ... Not used.
#' @return The final, validated, and complete `lavaan_plot_config` object. This
#'   object is the input for the next step in the workflow, `prepare()`.
#' @seealso \code{\link{analyze}}, \code{\link{prepare}}, \code{\link{build}}, \code{\link{render}}
#' @export
#' @examples
#' \dontrun{
#' # ...continuing from the analyze() example...
#' # Configure the plot with a few custom settings
#' plot_config <- configure_plot(analyzed_model,
#'                               plot_flow_direction = "LR",
#'                               text_size_global = 12,
#'                               node_labels = list(ind60 = "Industrialization"))
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

#' @title 3. Prepare Final Data for Plotting
#' @description This is the third phase of the `lavaanReportR` workflow. The
#'   `prepare` function takes the `lavaan_plot_config` object and applies all
#'   styling, labeling, and filtering rules. It produces the final, "build-ready"
#'   data tables for all nodes and edges.
#' @details
#' This phase is the final data transformation step. It enriches the analyzed
#' model structure with the aesthetic settings from the configuration, resulting
#' in a `lavaan_graph` object. This object contains everything needed to build the
#' DOT graph code in the next step.
#' @param x An object of class `lavaan_plot_config` from the `configure_plot()` phase.
#' @param ... Not used.
#' @return A `lavaan_graph` object containing the final, build-ready
#'   data.tables for nodes and edges, along with the final recipe. This object is
#'   the input for the next step in the workflow, `build()`.
#' @seealso \code{\link{analyze}}, \code{\link{configure_plot}}, \code{\link{build}}, \code{\link{render}}
#' @export
#' @examples
#' \dontrun{
#' # ...continuing from the configure_plot() example...
#' prepared_graph <- prepare(plot_config)
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

#' @title 4. Build DOT Code for Graphviz
#' @description This is the fourth phase of the `lavaanReportR` workflow. The
#'   `build` function takes the prepared `lavaan_graph` object and constructs the
#'   complete DOT language string required by Graphviz to render the path diagram.
#' @details
#' This function is a "dumb assembler." It translates the prepared data tables
#' from the `prepare` phase into DOT statements without making any further decisions.
#' The output is a `lavaan_dot_code` object, which is a simple wrapper around the
#' final DOT code string.
#' @param x An object of class `lavaan_graph` from the `prepare()` phase.
#' @param ... Not used.
#' @return A `lavaan_dot_code` object containing the final DOT string. This
#'   object is the input for the final step in the workflow, `render()`.
#' @seealso \code{\link{analyze}}, \code{\link{configure_plot}}, \code{\link{prepare}}, \code{\link{render}}
#' @export
#' @examples
#' \dontrun{
#' # ...continuing from the prepare() example...
#' dot_code <- build(prepared_graph)
#' cat(dot_code$dot_code) # You can inspect the generated DOT code
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

#' @title 5. Render the Final Plot
#' @description This is the final phase of the `lavaanReportR` workflow. The
#'   `render` function takes the `lavaan_dot_code` object and uses the `DiagrammeR`
#'   package to render the final graph.
#' @details
#' This is the simplest phase of the workflow. It acts as a lightweight wrapper
#' around `DiagrammeR::grViz()`, passing the generated DOT code to be rendered
#' into a final plot object.
#' @param x An object of class `lavaan_dot_code` from the `build()` phase.
#' @param ... Not used.
#' @return A `dgr_graph` object from the `DiagrammeR` package. This object can
#'   be printed to the console to display the plot or included in R Markdown
#'   documents.
#' @seealso \code{\link{analyze}}, \code{\link{configure_plot}}, \code{\link{prepare}}, \code{\link{build}}
#' @importFrom DiagrammeR grViz
#' @export
#' @examples
#' \dontrun{
#' # A complete, end-to-end example of the workflow
#' library(lavaan)
#'
#' model <- '
#'   # measurement model
#'     ind60 =~ x1 + x2 + x3
#'     dem60 =~ y1 + y2 + y3 + y4
#'     dem65 =~ y5 + y6 + y7 + y8
#'   # structural model
#'     dem60 ~ ind60
#'     dem65 ~ ind60 + dem60
#' '
#' fit <- sem(model, data = PoliticalDemocracy)
#' param_table <- lavaan::parameterEstimates(fit)
#'
#' # Run the full pipeline
#' final_plot <- param_table |>
#'   analyze() |>
#'   configure_plot(
#'     plot_flow_direction = "LR",
#'     node_labels = list(ind60 = "Industry", dem60 = "Democracy 1960")
#'   ) |>
#'   prepare() |>
#'   build() |>
#'   render()
#'
#' # Display the plot
#' final_plot
#' }
render.lavaan_dot_code <- function(x, ...) {
  # Die "dümmste" Phase: Nur noch den String an die Render-Engine übergeben.
  DiagrammeR::grViz(x$dot_code)
}
