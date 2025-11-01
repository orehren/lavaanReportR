#############################
# --- 1. Main Functions --- #
#############################

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
  render_map_nodes <- list(
    variances = NODE_TYPES$VARIANCE,
    intercepts = NODE_TYPES$INTERCEPT
  )
  prepared_data <- .filter_by_render_elements(
    prepared_data, "node_type", render_map_nodes, config$recipe
  )

  # --- 3. Early Exit ---
  if (nrow(prepared_data) == 0) {
    return(prepared_data)
  }

  # --- 4. Labeling ---
  value_col_name <- .get_estimate_col_name(config$user_args)
  prepared_data <- .determine_final_node_labels(
    nodes_data = prepared_data,
    user_labels = config$user_args$node_labels,
    value_col_name = value_col_name
  )

  # --- 5. Styling, Layout Enrichment, and Prefixing ---
  prepared_data <- .apply_node_styling(prepared_data, config$recipe)

  layout_info <- config$analyzed_model$layout
  layout_map <- data.table(
    id = unlist(layout_info$levels, use.names = FALSE),
    rank = rep(names(layout_info$levels), lengths(layout_info$levels))
  )
  prepared_data[layout_map, on = "id", rank := as.integer(i.rank)]

  if (nrow(layout_info$element_unit_map) > 0) {
    prepared_data[layout_info$element_unit_map,
      on = "id",
      `:=`(element_unit = i.element_unit, element_unit_order = i.element_unit_order)
    ]
  }

  prepared_data <- .apply_group_level_prefixes(prepared_data, "id", config)

  # --- 6. Final Sorting for Deterministic Layout ---
  # This is the new, correct location for the sorting logic.
  # It ensures the data passed to the build phase is already in the final order.
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
  prepared_data <- data.table::rbindlist(
    list(config$analyzed_model$edges, config$analyzed_model$defined_paths),
    use.names = TRUE, fill = TRUE
  )

  # --- 2. Generate Final Edge ID (NEU) ---
  # Erzeuge die eindeutige ID aus den Metadaten. Dies gehört in die prepare-Phase.
  prepared_data[, id := paste(id_prefix, from, "to", to, sep = "_")]

  # --- 3. Filtering ---
  render_map_edges <- list(
    variances = EDGE_TYPES$VARIANCE,
    intercepts = EDGE_TYPES$INTERCEPT,
    covariances = c(EDGE_TYPES$COVARIANCE)
  )
  prepared_data <- .filter_by_render_elements(
    prepared_data, "edge_type", render_map_edges, config$recipe
  )

  # --- 4. Labeling ---
  estimate_col <- .get_estimate_col_name(config$user_args)
  prepared_data <- .determine_final_edge_labels(
    edges_data = prepared_data,
    user_labels = config$user_args$effect_labels,
    recipe = config$recipe,
    estimate_col = estimate_col
  )

  # --- 5. Styling and Prefixing ---
  prepared_data <- .apply_edge_styling(prepared_data, config$recipe)
  prepared_data <- .apply_group_level_prefixes(prepared_data, c("from", "to"), config)

  # --- 6. Final Return ---
  return(prepared_data)
}


#' @title Apply Styling from Recipe to a Data Table
#' @description A universal factory that merges default styles with recipe
#'   overrides and joins them to a data table based on a type column.
#'   This function uses a functional approach with `lapply` and programmatically
#'   constructs the entire update-join expression for each style attribute,
#'   adhering to modern, safe `data.table` programming idioms.
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
  # 1. Combine default styles with user overrides
  user_overrides <- recipe$style_overrides %||% list()
  final_styles <- utils::modifyList(styles_constant, user_overrides)

  # 2. Convert style list to a data.table for joining
  styles_dt <- data.table::rbindlist(final_styles, fill = TRUE, idcol = type_col)

  # 3. Perform the update join for each style column using lapply
  style_col_names <- setdiff(names(styles_dt), type_col)

  lapply(style_col_names, function(col_name) {
    # For each column, create the specific `j` expression as a `call` object.
    # This creates a complete, valid language object, e.g., `shape := i.shape`
    j_call <- call(
      ":=",
      as.name(col_name),
      as.name(paste0("i.", col_name))
    )

    # Execute the update join, passing the entire j-call via env.
    # This is the safe, idiomatic, and eval-free way.
    dt[styles_dt, on = type_col, j = .j, env = list(.j = j_call)]
  })

  return(dt)
}


###############################
# --- 2. Helper Functions --- #
###############################


#' @title Determine Final Node Labels
#' @description Determines the final display label for each node. It takes the
#'   base label from the analysis phase, applies any user-defined overrides,
#'   and then sets the final label for special node types like variances and
#'   intercepts.
#'
#' @param nodes_data The input node data table, which must contain a 'label' column.
#' @param user_labels A named list of user-defined labels.
#' @param value_col_name The name of the column containing the numeric estimate.
#'
#' @return The `nodes_data` table with an updated `label` column.
#' @keywords internal
.determine_final_node_labels <- function(nodes_data, user_labels, value_col_name) {
  # Step 1: Apply user-defined labels directly to the existing 'label' column.
  # The `match_col` and `target_col` are the same.
  .apply_user_labels(
    dt = nodes_data,
    user_labels = user_labels,
    match_col = "label",
    target_col = "label"
  )

  # Step 2: Override labels for special node types (highest priority).
  nodes_data[
    node_type %in% c(NODE_TYPES$VARIANCE, NODE_TYPES$INTERCEPT),
    label := sprintf("%.2f", .SD[[1]]),
    .SDcols = value_col_name
  ]

  return(nodes_data)
}


#' @title Determine Final Edge Labels
#' @description Determines the final, information-rich display label for each edge.
#'   It correctly handles structural-only edges (no label) and ensures the
#'   equals sign is only shown when a text label is present.
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
  # # --- Step 0: Ensure data integrity ---
  # # This is the fix: Explicitly convert the estimate column to numeric.
  # # This handles NAs and any potential type coercion from the rbindlist step,
  # # making the subsequent mathematical operations safe.
  # if (estimate_col %in% names(edges_data)) {
  #   edges_data[, (estimate_col) := as.numeric(.SD[[1]]), .SDcols = estimate_col]
  # }

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

  # Define edge types that should never have a label
  structural_edge_types <- c(
    EDGE_TYPES$VARIANCE,
    EDGE_TYPES$INTERCEPT,
    EDGE_TYPES$MODERATED_PATH_SEGMENT_1
  )

  edges_data[, label := data.table::fcase(
    # HIGHEST PRIORITY: Structural edges get no label.
    edge_type %in% structural_edge_types,
    "",

    # Case 1: Show estimates AND a text part exists.
    show_estimates_condition & nzchar(text_part),
    paste(text_part, "=", estimate_part),

    # Case 2: Show estimates BUT NO text part exists.
    show_estimates_condition,
    estimate_part,

    # Default: Don't show estimates, just use the text part.
    default = text_part
  )]

  return(edges_data)
}


#' @title Apply Group and Level Prefixes to IDs
#' @description Prepends prefixes like "g1_", "l1_", or "g1_l2_" to ID columns
#'   if the model is multi-group or multi-level. The prefixes are based on the
#'   `split_hierarchy` defined in the recipe. This ensures all node and edge IDs
#'   are unique across different subgraphs.
#'
#' @param dt The data.table to modify (either nodes or edges).
#' @param id_cols A character vector of the column names to which prefixes
#'   should be prepended (e.g., "id" for nodes, c("from", "to") for edges).
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

  prefix_parts <- lapply(hierarchy, function(level_name) {
    prefix <- substr(level_name, 1, 1)
    paste0(prefix, dt[[level_name]] |> .sanitize_string())
  })

  prefix_string <- do.call(paste, c(prefix_parts, sep = "_"))

  lapply(id_cols, function(col) {
    dt[, (col) := paste(prefix_string, .SD[[col]], sep = "_")]
  })

  return(dt)
}


#' @title Apply Node Styling from Recipe
#' @description Merges the default node styles (`NODE_STYLES`) with any user-defined
#'   `style_overrides` from the recipe, and then joins these final styles to
#'   the nodes table based on `node_type`.
#' @param nodes_data The node data table.
#' @param recipe The final, complete recipe object.
#' @return The `nodes_data` table with added columns for each final style attribute.
#' @keywords internal
.apply_node_styling <- function(nodes_data, recipe) {
  .apply_styling(nodes_data, NODE_STYLES, recipe, "node_type")
}


#' @title Apply Edge Styling from Recipe
#' @description Merges default edge styles with any user-defined `style_overrides`
#'   from the recipe, and then joins these final styles to the edges table
#'   based on `edge_type`. This implementation uses a functional approach with
#'   `lapply` and programmatically constructs the update-join call for each
#'   style attribute, adhering to modern `data.table` programming idioms.
#'
#' @param edges_data The edge data table.
#' @param recipe The final, complete recipe object.
#'
#' @return The `edges_data` table with added style attribute columns.
#'
#' @keywords internal
#' @noRd
.apply_edge_styling <- function(edges_data, recipe) {
  .apply_styling(edges_data, EDGE_STYLES, recipe, "edge_type")
}


#' @title Apply User-Defined Labels to a Data Table
#' @description A universal helper that takes a data table and a named list of
#'   user labels, and updates a specified target column with these labels. It
#'   performs a robust update join.
#'
#' @param dt The data.table to modify (e.g., nodes_data or edges_data).
#' @param user_labels A named list where names correspond to the values in the
#'   `match_col` and values are the new labels.
#' @param match_col The name of the column in `dt` to match against the names
#'   of `user_labels`.
#' @param target_col The name of the column in `dt` that should be updated with
#'   the user labels.
#'
#' @return The modified data.table.
#' @keywords internal
#' @noRd
.apply_user_labels <- function(dt, user_labels, match_col, target_col) {
  # Guard Clause: Do nothing if no user labels are provided.
  if (length(user_labels) == 0) {
    return(dt)
  }

  user_labels_dt <- data.table::data.table(
    match = names(user_labels),
    user_label = as.character(user_labels)
  )

  # Perform the update join. The `on` argument is constructed programmatically
  # to match the specified `match_col`.
  dt[user_labels_dt, on = setNames("match", match_col), (target_col) := i.user_label]

  return(dt)
}


#' @title Filter Data Based on Render Elements from Recipe
#' @description A universal helper that filters a data table (nodes or edges)
#'   based on the `render_elements` setting in the recipe. It removes rows
#'   corresponding to types that should not be rendered.
#'
#' @param dt The data.table to filter (e.g., `prepared_data` for nodes or edges).
#' @param type_col The name of the column in `dt` that contains the type key
#'   (e.g., "node_type" or "edge_type").
#' @param render_map A named list that maps the string values from `render_elements`
#'   (e.g., "variances") to the corresponding type constants (e.g., `NODE_TYPES$VARIANCE`).
#' @param recipe The final, complete recipe object.
#'
#' @return The filtered data.table.
#' @keywords internal
#' @noRd
.filter_by_render_elements <- function(dt, type_col, render_map, recipe) {
  elements_to_render <- recipe$render_elements

  # Identify the types to remove by finding which keys in the map are NOT
  # present in the elements_to_render vector.
  types_to_remove <- unlist(render_map[!names(render_map) %in% elements_to_render])

  if (length(types_to_remove) > 0) {
    # Use a dynamic `data.table` subset. The `..()` prefix is used to evaluate
    # `type_col` from the function's environment.
    # dt <- dt[!.SD[[1]] %in% types_to_remove, .SD, .SDcols = type_col]
    dt <- dt[!(dt[[type_col]] %in% types_to_remove)]
  }

  return(dt)
}


#' @title Get the Name of the Estimate Column to Use
#' @description A simple helper that determines the correct estimate column name
#'   (e.g., "est.std" or "est.unstd") based on the `estimates_to_show` user argument.
#'
#' @param user_args A list of the final, processed user arguments.
#'
#' @return A character string with the name of the column.
#' @keywords internal
#' @noRd
.get_estimate_col_name <- function(user_args) {
  suffix <- if (user_args$estimates_to_show == "standardized") ".std" else ".unstd"
  paste0("est", suffix)
}


#' @title Format Mediator Nodes for Edge Labels
#' @description Takes a list of mediator nodes and formats it into a concise
#'   string for display in an edge label, e.g., "(M1)" or "(M1, M2)". This
#'   version uses `sprintf` for maximum clarity and readability.
#'
#' @param mediators A list where each element is a character vector of mediator names.
#'
#' @return A character vector of formatted mediator strings.
#' @keywords internal
#' @noRd
.format_mediators_for_label <- function(mediators) {
  sapply(mediators, function(m) {
    # Early exit for cases with no mediators
    if (length(m) == 0 || all(is.na(m))) {
      return("")
    }

    # Collapse the mediator names into a single string, e.g., "M1, M2"
    collapsed_mediators <- paste(m, collapse = ", ")

    # Use sprintf to apply the parentheses. This is clean and non-nested.
    sprintf("(%s)", collapsed_mediators)
  })
}
