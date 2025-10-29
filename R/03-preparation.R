# ==============================================================================
# SECTION: PREPARATION PHASE
# ==============================================================================
# This section contains functions for the "prepare" phase of the workflow.
# The goal of this phase is to take the analyzed model structure and the final
# configuration (recipe), and transform them into "build-ready" data tables.
# This involves applying styles, creating labels, filtering elements, and
# enriching the data with layout information.
# ==============================================================================

# ------------------------------------------------------------------------------
# Main Orchestrators for Node and Edge Preparation
# ------------------------------------------------------------------------------

#' @title Prepare Node Data for Build Phase
#' @description Orchestrates the transformation of analyzed node data into the
#'   final, "build-ready" node table by applying filtering, labeling, styling,
#'   layout enrichment, and prefixing logic.
#'
#' @param config The final `lavaan_plot_config` object.
#'
#' @return A data.table where each row is a fully prepared node, containing all
#'   necessary analysis and style attributes for the build phase.
#' @keywords internal
#' @noRd
.prepare_nodes_for_build <- function(config) {
  # --- 1. Initial Setup ---
  # Start with a full, clean copy of the analyzed node data.
  prepared_data <- copy(config$analyzed_model$nodes)

  # --- 2. Filtering ---
  # Remove nodes that the user has chosen to hide (e.g., variances).
  render_map_nodes <- list(
    variances = NODE_TYPES$VARIANCE,
    intercepts = NODE_TYPES$INTERCEPT
  )
  prepared_data <- .filter_by_render_elements(
    prepared_data, "node_type", render_map_nodes, config$recipe
  )

  # --- 3. Early Exit ---
  # If no nodes remain after filtering, there is nothing more to do.
  if (nrow(prepared_data) == 0) {
    return(prepared_data)
  }

  # --- 4. Labeling ---
  # Determine the final display labels for each node.
  value_col_name <- .get_estimate_col_name(config$user_args)
  prepared_data <- .determine_final_node_labels(
    nodes_data = prepared_data,
    user_labels = config$user_args$node_labels,
    value_col_name = value_col_name
  )

  # --- 5. Styling, Layout Enrichment, and Prefixing ---
  # Apply all aesthetic styles from the recipe (e.g., shape, color).
  prepared_data <- .apply_node_styling(prepared_data, config$recipe)

  # Add layout information (rank) to each node.
  layout_info <- config$analyzed_model$layout
  layout_map <- data.table(
    id = unlist(layout_info$levels, use.names = FALSE),
    rank = rep(names(layout_info$levels), lengths(layout_info$levels))
  )
  prepared_data[layout_map, on = "id", rank := i.rank]

  # Add group/level prefixes to node IDs if necessary.
  prepared_data <- .apply_group_level_prefixes(prepared_data, "id", config)

  # --- 6. Final Sorting for Deterministic Layout ---
  # Sorting the nodes ensures a consistent layout in the final plot.
  if ("rank" %in% names(prepared_data)) {
    data.table::setorderv(
      prepared_data,
      cols = c("rank", "node_unit")
    )
  }

  return(prepared_data)
}


#' @title Prepare Edge Data for Build Phase
#' @description Orchestrates the transformation of analyzed edge data into the
#'   final, "build-ready" edge table by applying filtering, labeling, styling,
#'   and prefixing logic.
#'
#' @param config The final `lavaan_plot_config` object.
#' @return A data.table where each row is a fully prepared edge.
#' @keywords internal
#' @noRd
.prepare_edges_for_build <- function(config) {
  # --- 1. Initial Setup ---
  # Combine standard edges and user-defined paths into a single table.
  prepared_data <- data.table::rbindlist(
    list(config$analyzed_model$edges, config$analyzed_model$defined_paths),
    use.names = TRUE, fill = TRUE
  )

  # --- 2. Generate Final Edge ID ---
  # Create a unique ID for each edge.
  prepared_data[, id := paste(id_prefix, from, "to", to, sep = "_")]

  # --- 3. Filtering ---
  # Remove edges that the user has chosen to hide.
  render_map_edges <- list(
    variances = EDGE_TYPES$VARIANCE,
    intercepts = EDGE_TYPES$INTERCEPT,
    covariances = c(EDGE_TYPES$COVARIANCE)
  )
  prepared_data <- .filter_by_render_elements(
    prepared_data, "edge_type", render_map_edges, config$recipe
  )

  # --- 4. Labeling ---
  # Determine the final display labels for each edge.
  estimate_col <- .get_estimate_col_name(config$user_args)
  prepared_data <- .determine_final_edge_labels(
    edges_data = prepared_data,
    user_labels = config$user_args$effect_labels,
    recipe = config$recipe,
    estimate_col = estimate_col
  )

  # --- 5. Styling and Prefixing ---
  # Apply all aesthetic styles from the recipe (e.g., color, linetype).
  prepared_data <- .apply_edge_styling(prepared_data, config$recipe)
  # Add group/level prefixes to `from` and `to` columns if necessary.
  prepared_data <- .apply_group_level_prefixes(prepared_data, c("from", "to"), config)

  return(prepared_data)
}

# ------------------------------------------------------------------------------
# Universal Styling and Helper Functions
# ------------------------------------------------------------------------------

#' @title Apply Styling from Recipe to a Data Table
#' @description A universal factory that merges default styles with recipe
#'   overrides and joins them to a data table based on a type column.
#'
#' @param dt The data.table to apply styling to (nodes or edges).
#' @param styles_constant The style constant to use (`NODE_STYLES` or `EDGE_STYLES`).
#' @param recipe The final, complete recipe object.
#' @param type_col The name of the column in `dt` that contains the type key
#'   (e.g., "node_type" or "edge_type").
#'
#' @return The `dt` with added style attribute columns.
#' @keywords internal
#' @noRd
.apply_styling <- function(dt, styles_constant, recipe, type_col) {
  # 1. Combine default styles with user overrides from the recipe.
  user_overrides <- recipe$style_overrides %||% list()
  final_styles <- utils::modifyList(styles_constant, user_overrides)

  # 2. Convert the style list to a data.table for efficient joining.
  styles_dt <- data.table::rbindlist(final_styles, fill = TRUE, idcol = type_col)

  # 3. Perform the update join for each style attribute.
  style_col_names <- setdiff(names(styles_dt), type_col)

  lapply(style_col_names, function(col_name) {
    # Programmatically construct the update expression (e.g., `shape := i.shape`).
    j_call <- call(
      ":=",
      as.name(col_name),
      as.name(paste0("i.", col_name))
    )
    # Execute the update join safely.
    dt[styles_dt, on = type_col, j = .j, env = list(.j = j_call)]
  })

  return(dt)
}

#' @title Determine Final Node Labels
#' @description Determines the final display label for each node. It applies
#'   user-defined overrides and formats labels for special node types.
#'
#' @param nodes_data The input node data table.
#' @param user_labels A named list of user-defined labels.
#' @param value_col_name The name of the column containing the numeric estimate.
#'
#' @return The `nodes_data` table with an updated `label` column.
#' @keywords internal
.determine_final_node_labels <- function(nodes_data, user_labels, value_col_name) {
  # Step 1: Apply any user-provided labels first.
  .apply_user_labels(
    dt = nodes_data,
    user_labels = user_labels,
    match_col = "label",
    target_col = "label"
  )

  # Step 2: Override labels for special node types (e.g., variances).
  nodes_data[
    node_type %in% c(NODE_TYPES$VARIANCE, NODE_TYPES$INTERCEPT),
    label := sprintf("%.2f", .SD[[1]]),
    .SDcols = value_col_name
  ]

  return(nodes_data)
}

#' @title Determine Final Edge Labels
#' @description Determines the final, information-rich display label for each edge.
#'
#' @param edges_data The input edge data table.
#' @param user_labels A named list of user-defined labels for effects.
#' @param recipe The final, complete recipe object.
#' @param estimate_col The name of the column containing the numeric estimate.
#'
#' @return The `edges_data` table with a correctly formatted `label` column.
#' @keywords internal
#' @noRd
.determine_final_edge_labels <- function(edges_data, user_labels, recipe, estimate_col) {
  # --- Step 1: Apply user-defined labels ---
  .apply_user_labels(
    dt = edges_data,
    user_labels = user_labels,
    match_col = "label",
    target_col = "label"
  )

  # --- Step 2: Construct the component parts of the label ---
  structural_part <- .format_mediators_for_label(edges_data$mediators)
  est_string <- sprintf("%.2f", edges_data[[estimate_col]])
  sig_string <- if (recipe$show_sig) edges_data$sig else ""
  estimate_part <- trimws(paste0(est_string, sig_string))
  text_part <- trimws(paste(edges_data$label, structural_part))

  # --- Step 3: Assemble the final label using a robust, flat fcase ---
  show_estimates_condition <- recipe$show_estimates &
    !is.na(edges_data[[estimate_col]]) &
    abs(edges_data[[estimate_col]]) >= recipe$show_estimates_above

  # Define edge types that should never have a label (e.g., variances).
  structural_edge_types <- c(
    EDGE_TYPES$VARIANCE,
    EDGE_TYPES$INTERCEPT,
    EDGE_TYPES$MODERATED_PATH_SEGMENT_1
  )

  edges_data[, label := data.table::fcase(
    edge_type %in% structural_edge_types, "",
    show_estimates_condition & nzchar(text_part), paste(text_part, "=", estimate_part),
    show_estimates_condition, estimate_part,
    default = text_part
  )]

  return(edges_data)
}

#' @title Apply Group and Level Prefixes to IDs
#' @description Prepends prefixes (e.g., "g1_", "l1_") to ID columns if the
#'   model is multi-group or multi-level, ensuring unique IDs across subgraphs.
#'
#' @param dt The data.table to modify (nodes or edges).
#' @param id_cols The column names to which prefixes should be prepended.
#' @param config The final `lavaan_plot_config` object.
#'
#' @return The modified data.table with prefixed IDs.
#' @keywords internal
#' @noRd
.apply_group_level_prefixes <- function(dt, id_cols, config) {
  hierarchy <- config$recipe$split_hierarchy
  if (length(hierarchy) == 0) {
    return(dt)
  }

  # Create the prefix string (e.g., "g1_l2").
  prefix_parts <- lapply(hierarchy, function(level_name) {
    prefix <- substr(level_name, 1, 1)
    paste0(prefix, dt[[level_name]] |> .sanitize_string())
  })
  prefix_string <- do.call(paste, c(prefix_parts, sep = "_"))

  # Apply the prefix to the specified ID columns.
  lapply(id_cols, function(col) {
    dt[, (col) := paste(prefix_string, .SD[[col]], sep = "_")]
  })

  return(dt)
}

#' @title Apply User-Defined Labels to a Data Table
#' @description A universal helper that updates a target column with user-defined labels.
#'
#' @param dt The data.table to modify.
#' @param user_labels A named list of user labels.
#' @param match_col The column in `dt` to match against the names of `user_labels`.
#' @param target_col The column in `dt` to update with the user labels.
#'
#' @return The modified data.table.
#' @keywords internal
#' @noRd
.apply_user_labels <- function(dt, user_labels, match_col, target_col) {
  if (length(user_labels) == 0) {
    return(dt)
  }

  user_labels_dt <- data.table::data.table(
    match = names(user_labels),
    user_label = as.character(user_labels)
  )

  # Perform a robust update join.
  dt[user_labels_dt, on = setNames("match", match_col), (target_col) := i.user_label]
  return(dt)
}
