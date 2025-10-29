# ==============================================================================
# SECTION: ANALYSIS HELPERS - FEATURE DETECTION
# ==============================================================================
# This section contains functions dedicated to analyzing the raw parameter table
# from lavaan to detect high-level features of the SEM model. This includes
# identifying the model type (e.g., CFA, Path, LGM), and checking for complex
# structures like multigroup, multilevel, mediation, or moderation.
# ==============================================================================

# ------------------------------------------------------------------------------
# Main Orchestrator for Feature Analysis
# ------------------------------------------------------------------------------

#' @title Analyze Model Features (Orchestrator)
#' @description Orchestrates the detection of various model features. It first
#'   collects general features, then performs a detailed LGM analysis, and
#'   finally determines the definitive model type and default hierarchy.
#' @param param_table The parameter data.table.
#' @return A named list of all detected model features, including the detailed
#'   `$lgm_analysis` result if applicable.
#' @keywords internal
#' @noRd
.analyze_model_features <- function(param_table) {
  # --- 1. Collect general features ---
  # Gather a baseline understanding of the model's characteristics.
  general_features <- .collect_general_features(param_table)

  # --- 2. Perform the complete LGM analysis ONCE ---
  # This function returns a detailed list or NULL if not an LGM. This analysis
  # is crucial for layout and styling decisions later on.
  lgm_analysis <- .analyze_lgm_structure(param_table, general_features$has_latent)

  # --- 3. Determine the final model type ---
  # Start with the base type and refine it if an LGM was detected.
  base_type <- .determine_base_model_type(is_latent = general_features$has_latent)
  final_model_type <- lgm_analysis$lgm_type %||% base_type

  # --- 4. Determine default split hierarchy ---
  # Establishes the default nesting order for multigroup/multilevel models.
  default_hierarchy <- .determine_default_split_hierarchy(general_features)

  # --- 5. Assemble and return the final features list ---
  # We pass the full `lgm_analysis` object through, as it contains the
  # valuable `element_groups` needed for the layout calculation.
  c(
    list(
      model_type = final_model_type,
      default_split_hierarchy = default_hierarchy,
      lgm_analysis = lgm_analysis
    ),
    general_features
  )
}

# ------------------------------------------------------------------------------
# Atomic Helpers for General Feature Analysis
# ------------------------------------------------------------------------------

#' @title Collect General Model Features
#' @description A helper that bundles the results of several atomic checkers.
#' @param param_table The parameter data.table.
#' @return A named list of logical feature flags.
#' @keywords internal
#' @noRd
.collect_general_features <- function(param_table) {
  list(
    has_latent = .check_if_latent(param_table),
    has_group = .check_if_multigroup(param_table),
    has_level = .check_if_multilevel(param_table),
    has_mediation = .check_if_mediation(param_table),
    has_moderation = .check_if_moderation(param_table)
  )
}

#' @title Check for Latent Variables
#' @param param_table The parameter data.table.
#' @return A logical value.
#' @keywords internal
#' @noRd
.check_if_latent <- function(param_table) {
  # A model is considered to have latent variables if there are any factor loadings.
  MODEL_OPS$LOADINGS %in% param_table$op
}

#' @title Check for Multiple Groups
#' @param param_table The parameter data.table.
#' @return A logical value.
#' @keywords internal
#' @noRd
.check_if_multigroup <- function(param_table) {
  # A model is multigroup if a 'group' column exists and has more than one unique value.
  "group" %in% names(param_table) && data.table::uniqueN(param_table$group) > 1
}

#' @title Check for Multiple Levels
#' @param param_table The parameter data.table.
#' @return A logical value.
#' @keywords internal
#' @noRd
.check_if_multilevel <- function(param_table) {
  # A model is multilevel if a 'level' column exists and has more than one unique value.
  "level" %in% names(param_table) && data.table::uniqueN(param_table$level) > 1
}

#' @title Check for Mediation/Defined Parameters
#' @param param_table The parameter data.table.
#' @return A logical value.
#' @keywords internal
#' @noRd
.check_if_mediation <- function(param_table) {
  # Mediation or other user-defined effects are present if the ':=' operator is used.
  MODEL_OPS$DEFINED %in% param_table$op
}

#' @title Check for Moderation
#' @param param_table The parameter data.table.
#' @return A logical value.
#' @keywords internal
#' @noRd
.check_if_moderation <- function(param_table) {
  # Moderation is detected by the presence of an interaction term (containing ':').
  any(param_table$rhs %like% ":")
}

#' @title Determine Base Model Type
#' @param is_latent A logical value from `.check_if_latent`.
#' @return A character string: "PATH_MODEL" or "CFA_MODEL".
#' @keywords internal
#' @noRd
.determine_base_model_type <- function(is_latent) {
  # The base distinction is between models with and without latent variables.
  if (is_latent) "CFA_MODEL" else "PATH_MODEL"
}

#' @title Determine Default Split Hierarchy
#' @description Determines the default order for splitting multigroup/multilevel models.
#' @param features A list of general features.
#' @return A character vector (e.g., c("group", "level")).
#' @keywords internal
#' @noRd
.determine_default_split_hierarchy <- function(features) {
  # Creates a compact vector of the hierarchy, ordered by group then level by default.
  compact(c(
    if (features$has_group) "group",
    if (features$has_level) "level"
  ))
}

# ------------------------------------------------------------------------------
# LGM Detection Helpers
# ------------------------------------------------------------------------------

#' @title Analyze LGM Structure Across Subsets
#' @description A sub-orchestrator that splits the parameter table by group/level
#'   and applies the core LGM component analysis to each subset. It returns the
#'   analysis result for the first subset that is identified as an LGM.
#' @param param_table The full parameter data.table.
#' @param has_latent A logical flag from the general feature analysis.
#' @return A list of LGM analysis results, or `NULL` if no subset is an LGM.
#' @keywords internal
#' @noRd
.analyze_lgm_structure <- function(param_table, has_latent) {
  # Guard clause: An LGM must have latent variables.
  if (!has_latent) {
    return(NULL)
  }

  # Split the data by any grouping factors to analyze each sub-model independently.
  split_cols <- intersect(c("group", "level"), names(param_table))
  param_subsets <- split(param_table, by = split_cols)

  # Apply the LGM component analysis to each subset.
  lgm_results_per_subset <- map(
    param_subsets,
    ~ .analyze_lgm_components(.x)
  )

  # Return the analysis for the first subset that is confirmed to be an LGM.
  detect(lgm_results_per_subset, ~ .x$is_lgm)
}

#' @title Analyze LGM Components (Orchestrator)
#' @description Orchestrates the analysis of LGM components through a fail-fast
#'   pipeline. It calls a series of atomic helpers to identify all components
#'   of a Latent Growth Model in a clean, sequential workflow.
#' @param param_table The parameter data.table for a single sub-model.
#' @return A list with LGM analysis results (`$is_lgm`, `$lgm_type`,
#'   `$element_groups`), or a list with `$is_lgm = FALSE` if not an LGM.
#' @keywords internal
#' @noRd
.analyze_lgm_components <- function(param_table) {
  # An LGM requires at least 3 time points, which means at least 3 loadings.
  loadings <- param_table[op == MODEL_OPS$LOADINGS, .(factor = lhs, indicator = rhs)]
  if (nrow(loadings) < 3) {
    return(list(is_lgm = FALSE))
  }

  regressions <- param_table[op == MODEL_OPS$REGRESSIONS, .(predictor = rhs, outcome = lhs)]
  all_latent_vars <- unique(loadings$factor)
  all_indicators <- unique(loadings$indicator)

  # --- Stage 2: Identification of Core LGM Components (Fail-Fast) ---
  # The identification process is a pipeline; if a key component is missing,
  # the function exits early.
  timepoints <- .find_lgm_timepoints(loadings, all_latent_vars, all_indicators)
  if (is.null(timepoints)) {
    return(list(is_lgm = FALSE))
  }

  growth_factors <- .find_lgm_growth_factors(loadings, timepoints)
  if (is.null(growth_factors)) {
    return(list(is_lgm = FALSE))
  }

  # --- Stage 3: Identification of all other components ---
  # Once the core components are found, identify the remaining elements.
  latent_timepoints <- intersect(timepoints, all_latent_vars)
  measurement_occasions <- setdiff(all_indicators, all_latent_vars)
  predictors <- .find_lgm_predictors(regressions, growth_factors, timepoints)
  tv_covariates <- .find_lgm_tv_covariates(regressions, measurement_occasions)

  # --- Stage 4: Final Assembly ---
  # Group the identified elements into semantic categories for layout.
  element_groups <- .assemble_lgm_element_groups(
    predictors, growth_factors, timepoints, latent_timepoints,
    measurement_occasions, tv_covariates
  )
  lgm_type <- .determine_final_lgm_type(timepoints, latent_timepoints)

  # --- Stage 5: Return all results ---
  list(
    is_lgm = TRUE,
    lgm_type = lgm_type,
    element_groups = compact(element_groups)
  )
}


#' @title Find LGM Timepoints (First-Order Growth Factors)
#' @description Identifies the repeated measures (timepoints) in a potential LGM.
#'   Fails (returns NULL) if the minimum count for an LGM (3) is not met.
#' @param loadings A data.table of loadings.
#' @param all_latent_vars A character vector of all latent variables.
#' @param all_indicators A character vector of all indicators.
#' @return A character vector of timepoint names, or `NULL`.
#' @keywords internal
#' @noRd
.find_lgm_timepoints <- function(loadings, all_latent_vars, all_indicators) {
  # Timepoints can be either latent (if they load on other indicators) or manifest.
  by_factor <- intersect(all_indicators, all_latent_vars)

  timepoints <- if (length(by_factor) > 0) {
    by_factor
  } else {
    # If no latent timepoints, find manifest variables that are loaded upon multiple times.
    loadings[, .N, by = indicator][N > 1, indicator]
  }

  # An LGM requires at least 3 timepoints.
  if (length(timepoints) < 3) {
    return(NULL)
  }
  return(timepoints)
}

#' @title Find LGM Growth Factors (Second-Order Factors)
#' @description Identifies the latent factors that load on the timepoints.
#'   Fails (returns NULL) if the minimum count for an LGM (2) is not met.
#' @param loadings A data.table of loadings.
#' @param timepoints A character vector of timepoint names.
#' @return A character vector of growth factor names, or `NULL`.
#' @keywords internal
#' @noRd
.find_lgm_growth_factors <- function(loadings, timepoints) {
  # Growth factors are the latent variables that predict the timepoints.
  factors <- unique(loadings[indicator %in% timepoints, factor])

  # An LGM requires at least 2 growth factors (e.g., intercept and slope).
  if (length(factors) < 2) {
    return(NULL)
  }
  return(factors)
}

#' @title Find LGM Predictors
#' @description Identifies time-invariant predictors of the growth factors.
#' @param regressions A data.table of regressions.
#' @param growth_factors A character vector of growth factor names.
#' @param timepoints A character vector of timepoint names.
#' @return A character vector of predictor names.
#' @keywords internal
#' @noRd
.find_lgm_predictors <- function(regressions, growth_factors, timepoints) {
  # Predictors are variables that predict the growth factors but are not timepoints themselves.
  setdiff(
    unique(regressions[outcome %in% growth_factors, predictor]),
    timepoints
  )
}

#' @title Find LGM Time-Varying Covariates
#' @description Identifies covariates that predict outcomes at specific time points.
#' @param regressions A data.table of regressions.
#' @param measurement_occasions A character vector of measurement occasion names.
#' @return A character vector of time-varying covariate names.
#' @keywords internal
#' @noRd
.find_lgm_tv_covariates <- function(regressions, measurement_occasions) {
  # TVCs predict the measurement occasions directly.
  unique(regressions[outcome %in% measurement_occasions, predictor])
}

#' @title Assemble LGM Element Groups
#' @description Takes all identified LGM components and assembles them into the
#'   final list of element groups, performing final set operations for cleaning.
#' @param predictors,growth_factors,... Character vectors of identified components.
#' @return A named list of the final element groups.
#' @keywords internal
#' @noRd
.assemble_lgm_element_groups <- function(predictors, growth_factors, timepoints,
                                         latent_timepoints, measurement_occasions,
                                         tv_covariates) {
  # Clean up the lists to ensure no overlap between categories.
  list(
    predictors = setdiff(predictors, tv_covariates),
    growth_factors = setdiff(growth_factors, timepoints),
    timepoint_factors = latent_timepoints,
    measurement_occasions = measurement_occasions,
    tv_covariates = tv_covariates
  )
}

#' @title Determine Final LGM Type
#' @description Classifies the LGM as either latent or manifest based on its timepoints.
#' @param timepoints A character vector of all timepoints.
#' @param latent_timepoints A character vector of latent timepoints.
#' @return A character string: "LGM_LATENT_MODEL" or "LGM_MANIFEST_MODEL".
#' @keywords internal
#' @noRd
.determine_final_lgm_type <- function(timepoints, latent_timepoints) {
  # If all timepoints are themselves latent variables, it's a latent LGM.
  if (length(latent_timepoints) == length(timepoints)) {
    "LGM_LATENT_MODEL"
  } else {
    "LGM_MANIFEST_MODEL"
  }
}

# ------------------------------------------------------------------------------
# Helpers for Defined Parameter Analysis
# ------------------------------------------------------------------------------
# This section focuses on deconstructing user-defined parameters (e.g., indirect
# or total effects specified with the ':=' operator). The goal is to understand
# the underlying structure of these complex paths for correct visualization.
# ------------------------------------------------------------------------------

#' @title Recursively Get Atomic Path Parts
#' @description A recursive helper function that deconstructs a user-defined
#'   parameter formula into its constituent atomic (base) path labels. For example,
#'   it will break down `"c + a*b"` into `c("c", "a", "b")`.
#'
#' @param path_label The label of the path to deconstruct (e.g., "total_c").
#' @param dt The full parameter data.table, used as a lookup dictionary for the recursion.
#'
#' @return A character vector of the atomic path labels that make up the formula.
#' @keywords internal
#' @noRd
.resolve_to_path_labels <- function(path_label, dt) {
  # Look up the definition for the current path label.
  row_data <- dt[label == path_label]

  # --- Base Cases for Recursion Termination ---

  # Guard 1: The label is not a defined parameter or has no formula.
  if (nrow(row_data) == 0 || row_data$rhs == "") {
    return(path_label)
  }

  # Guard 2: The label refers to a simple, atomic path (e.g., a regression).
  is_atomic_path <- row_data$op %in% c(MODEL_OPS$REGRESSIONS, MODEL_OPS$LOADINGS)
  if (is_atomic_path) {
    return(row_data$label)
  }

  # Guard 3: The "definition" is not a complex := operation. Treat as atomic.
  if (row_data$op != MODEL_OPS$DEFINED) {
    return(path_label)
  }

  # --- Recursive Step ---
  # Deconstruct the formula and recursively resolve each part.
  parts <- strsplit(row_data$rhs, "[-+*()]") |> unlist()
  parts <- trimws(parts)
  parts <- parts[nzchar(parts)]

  result_list <- lapply(parts, .resolve_to_path_labels, dt = dt)
  return(unique(unlist(result_list)))
}


#' @title Deconstruct Defined Paths and Enrich with Structure
#' @description This is the first core step in the defined path analysis pipeline.
#'   It takes the raw `defined_params` table, recursively deconstructs each
#'   parameter's formula, and enriches the result with `from` and `to` node info.
#'
#' @param defined_params A `data.table` containing only the `:=` parameter rows.
#' @param direct_paths A lookup `data.table` mapping base path labels to their nodes.
#' @param full_param_table The complete parameter table for recursive lookup.
#'
#' @return A long-format `data.table` with one row per base path within each
#'   defined parameter, including `from` and `to` node information.
#' @keywords internal
#' @noRd
.deconstruct_defined_paths <- function(defined_params, direct_paths, full_param_table) {
  # Deconstruct each defined parameter's formula into a long list of base path labels.
  deconstructed_long <- defined_params[, .(
    base_path_label = unlist(lapply(label, .resolve_to_path_labels, dt = full_param_table))
  ), by = .(defined_label = label)]

  # Join the `from` and `to` information for each base path.
  # An inner join is used here to drop parts of the formula that are not actual
  # paths in the model (e.g., numeric constants).
  paths_with_structure <- direct_paths[deconstructed_long, on = .(base_path_label), nomatch = NULL]
  return(paths_with_structure)
}

#' @title Aggregate Path Structure and Extract Mediators
#' @description The second core step in the pipeline. It aggregates the
#'   long-format deconstructed path data back to a single row per defined parameter,
#'   calculating the start/end nodes and identifying any mediators.
#'
#' @param paths_with_structure A long-format `data.table` from `.deconstruct_defined_paths`.
#' @return A `data.table` with one row per `defined_label`, containing the
#'   aggregated structure: `from`, `to`, `mediators`, and `base_paths`.
#' @keywords internal
#' @noRd
.aggregate_path_structure <- function(paths_with_structure) {
  paths_with_structure[,
    {
      all_nodes <- unique(c(from, to))
      start_nodes <- setdiff(from, to)
      end_nodes <- setdiff(to, from)

      .(
        from = start_nodes,
        to = end_nodes,
        mediators = list(setdiff(all_nodes, c(start_nodes, end_nodes))),
        base_paths = list(unique(base_path_label))
      )
    },
    by = .(defined_label)
  ]
}

#' @title Assemble Final Defined Paths Data
#' @description The final step in the pipeline. It joins the aggregated path
#'   structure information back to the original statistical and grouping data
#'   from the `defined_params` table.
#'
#' @param path_structure_info Aggregated structure from `.aggregate_path_structure`.
#' @param defined_params Original `:=` parameter rows with statistical metadata.
#' @param expanded_value_columns Names of all value columns to keep.
#' @param group_cols Names of grouping columns to keep.
#'
#' @return The final, complete `data.table` for all defined paths.
#' @keywords internal
#' @noRd
.assemble_defined_paths <- function(path_structure_info, defined_params, expanded_value_columns, group_cols) {
  # Select the relevant columns to keep from the original defined parameters table.
  cols_to_keep <- intersect(
    c("label", "rhs", "sig", "edge_type", group_cols, expanded_value_columns),
    names(defined_params)
  )

  # Join the aggregated structure back to the original metadata.
  final_data <- path_structure_info[defined_params[, ..cols_to_keep], on = .(defined_label = label)]

  # Clean up column names and create a unique ID.
  setnames(final_data, "defined_label", "label")
  final_data[, id := paste(substr(edge_type, 1, 3), .sanitize_string(label), sep = "_")]

  return(final_data)
}


#' @title Analyze Structure of Defined Paths (Orchestrator)
#' @description Orchestrates the analysis of user-defined parameters (`:=`) through
#'   a clear, multi-phase pipeline.
#'
#' @param param_table The full parameter data.table.
#' @param value_columns Base value column names to extract (e.g., "est").
#'
#' @return A final data.table for all defined paths, containing their structure
#'   (from, to, mediators) and all associated statistical data.
#' @keywords internal
#' @noRd
.analyze_defined_paths_structure <- function(param_table, value_columns = "est") {
  # --- 1. Preparation ---
  defined_params <- param_table[op == MODEL_OPS$DEFINED]
  if (nrow(defined_params) == 0) {
    return(data.table::data.table())
  }
  setnames(defined_params, "lhs", "label")

  # Classify the type of defined path based on the formula operators.
  defined_params[, edge_type := data.table::fcase(
    rhs %like% "\\+" & rhs %like% "\\*", SEMANTIC_TYPES$TOTAL,
    rhs %like% "\\+", SEMANTIC_TYPES$TOTAL,
    rhs %like% "-", SEMANTIC_TYPES$CONTRAST,
    rhs %like% "\\*", SEMANTIC_TYPES$INDIRECT,
    default = SEMANTIC_TYPES$DIRECT
  )]

  # Create a lookup table for all direct, labeled paths in the model.
  direct_paths <- param_table[
    op %in% c(MODEL_OPS$REGRESSIONS, MODEL_OPS$LOADINGS) & nzchar(as.character(label)),
    .(base_path_label = as.character(label), from = rhs, to = lhs)
  ]

  expanded_value_columns <- .expand_names_with_suffix(value_columns, c("std", "unstd"), ".")
  group_cols <- intersect(c("group", "level"), names(param_table))

  # --- 2. Main Pipeline ---
  # This pipeline clearly shows the data flow: deconstruct, aggregate, and assemble.
  defined_params |>
    .deconstruct_defined_paths(direct_paths = direct_paths, full_param_table = param_table) |>
    .aggregate_path_structure() |>
    .assemble_defined_paths(
      defined_params = defined_params,
      expanded_value_columns = expanded_value_columns,
      group_cols = group_cols
    )
}

# ------------------------------------------------------------------------------
# Helpers for Layout Analysis
# ------------------------------------------------------------------------------
# This section contains functions responsible for determining the hierarchical
# layout of the model graph. It uses the `igraph` package to perform a
# topological sort, ensuring that nodes are arranged in a logical, readable
# hierarchy from predictors to outcomes.
# ------------------------------------------------------------------------------

#' @title Calculate Hierarchical Structure
#' @description The core layout algorithm using `igraph`. It determines the
#'   hierarchical levels of nodes in a directed acyclic graph.
#'
#' @param nodes A character vector of node IDs.
#' @param edges A data.table with `from` and `to` columns for directed edges.
#'
#' @return A named list where names are level numbers (as characters) and
#'   values are character vectors of the node IDs on that level.
#' @keywords internal
#' @noRd
.calculate_hierarchical_structure <- function(nodes, edges) {
  graph <- igraph::graph_from_data_frame(edges, directed = TRUE, vertices = nodes)

  # A hierarchical layout is only possible for a Directed Acyclic Graph (DAG).
  if (!igraph::is_dag(graph)) {
    warning("A cycle was detected in the model graph. Hierarchical layout may be incorrect.", call. = FALSE)
    return(list(`1` = nodes))
  }

  # --- Core Algorithm ---
  # 1. Get a topologically sorted list of nodes.
  sorted_nodes <- igraph::topo_sort(graph, mode = "out") |> names()

  # 2. Initialize the level for all nodes to 1.
  node_names <- igraph::V(graph) |> names()
  node_levels <- rep(1, length(node_names)) |> stats::setNames(node_names)

  # 3. Iterate backwards through the sorted list to calculate levels.
  #    The level of a node is determined by the maximum level of its successors.
  for (node in rev(sorted_nodes)) {
    successors <- igraph::neighbors(graph, node, mode = "out") |> names()
    if (length(successors) == 0) {
      next # Sink nodes remain at level 1.
    }
    node_levels[node] <- 1 + max(node_levels[successors])
  }

  # 4. Invert the levels so that source nodes start at level 1.
  final_levels <- max(node_levels) - node_levels + 1

  # 5. Group nodes by their final level number.
  final_levels |>
    names() |>
    split(f = final_levels)
}

#' @title Analyze Graph Layout (Orchestrator)
#' @description Orchestrates the calculation of hierarchical layout levels for
#'   all nodes in the model. It uses a robust, two-step approach that correctly
#'   consolidates ranks for all semantic node units.
#'
#' @param nodes The data.table of all extracted nodes.
#' @param edges The data.table of all extracted edges.
#'
#' @return A named list containing the final layout data (`$levels`).
#' @keywords internal
#' @noRd
.analyze_layout_structure <- function(nodes, edges) {
  all_node_ids <- nodes$id
  directed_paths <- .collect_all_directed_paths(edges)
  unique_node_ids <- unique(all_node_ids)

  graph <- igraph::graph_from_data_frame(directed_paths, directed = TRUE, vertices = unique_node_ids)

  if (!igraph::is_dag(graph)) {
    warning("A cycle was detected...", call. = FALSE)
    return(list(levels = list(`1` = unique_node_ids), element_unit_map = data.table()))
  }

  # --- Initial Rank Calculation ---
  sorted_nodes <- names(igraph::topo_sort(graph, mode = "out"))
  node_levels <- stats::setNames(rep(1, length(unique_node_ids)), unique_node_ids)

  for (node in rev(sorted_nodes)) {
    successors <- names(igraph::neighbors(graph, node, mode = "out"))
    if (length(successors) > 0) {
      node_levels[node] <- 1 + max(node_levels[successors])
    }
  }
  initial_ranks <- max(node_levels) - node_levels + 1

  # --- Consolidate Ranks by Semantic Node Unit ---
  # This ensures that all parts of a complex node (e.g., a variable and its
  # variance) are placed on the same rank in the final layout.
  node_rank_map <- data.table(id = names(initial_ranks), initial_rank = initial_ranks)
  node_rank_map[nodes, on = "id", node_unit := i.node_unit]

  # The "master" rank for a unit is the rank of its main node.
  master_ranks <- node_rank_map[
    id == .sanitize_string(node_unit),
    .(node_unit, master_rank = initial_rank)
  ]

  # Apply the master rank to all nodes within that unit.
  node_rank_map[master_ranks, on = "node_unit", final_rank := i.master_rank]

  # Assemble the final layout list.
  final_levels <- split(node_rank_map$id, f = node_rank_map$final_rank)

  return(list(levels = final_levels, element_unit_map = data.table()))
}
