# ==============================================================================
# SECTION: ANALYSIS HELPERS - FEATURE DETECTION
# ==============================================================================

# ------------------------------------------------------------------------------
# Main Orchestrator for Feature Analysis
# ------------------------------------------------------------------------------pa

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
  general_features <- .collect_general_features(param_table)

  # --- 2. Perform the complete LGM analysis ONCE ---
  # This function returns a detailed list or NULL if not an LGM.
  lgm_analysis <- .analyze_lgm_structure(param_table, general_features$has_latent)

  # --- 3. Determine the final model type ---
  # Start with the base type and refine it if an LGM was detected.
  base_type <- .determine_base_model_type(is_latent = general_features$has_latent)
  final_model_type <- lgm_analysis$lgm_type %||% base_type

  # --- 4. Determine default split hierarchy ---
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
  MODEL_OPS$LOADINGS %in% param_table$op
}

#' @title Check for Multiple Groups
#' @param param_table The parameter data.table.
#' @return A logical value.
#' @keywords internal
#' @noRd
.check_if_multigroup <- function(param_table) {
  "group" %in% names(param_table) && data.table::uniqueN(param_table$group) > 1
}

#' @title Check for Multiple Levels
#' @param param_table The parameter data.table.
#' @return A logical value.
#' @keywords internal
#' @noRd
.check_if_multilevel <- function(param_table) {
  "level" %in% names(param_table) && data.table::uniqueN(param_table$level) > 1
}

#' @title Check for Mediation/Defined Parameters
#' @param param_table The parameter data.table.
#' @return A logical value.
#' @keywords internal
#' @noRd
.check_if_mediation <- function(param_table) {
  MODEL_OPS$DEFINED %in% param_table$op
}


#' @title Check for Moderation
#' @param param_table The parameter data.table.
#' @return A logical value.
#' @keywords internal
#' @noRd
.check_if_moderation <- function(param_table) {
  any(param_table$rhs %like% ":")
}

#' @title Determine Base Model Type
#' @param is_latent A logical value from `.check_if_latent`.
#' @return A character string: "PATH_MODEL" or "CFA_MODEL".
#' @keywords internal
#' @noRd
.determine_base_model_type <- function(is_latent) {
  if (is_latent) "CFA_MODEL" else "PATH_MODEL"
}

#' @title Determine Default Split Hierarchy
#' @description Determines the default order for splitting multigroup/multilevel models.
#' @param features A list of general features.
#' @return A character vector (e.g., c("group", "level")).
#' @keywords internal
#' @noRd
.determine_default_split_hierarchy <- function(features) {
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
  # Dieser Guard ist wichtig und korrekt.
  if (!has_latent) {
    return(NULL)
  }

  split_cols <- intersect(c("group", "level"), names(param_table))
  param_subsets <- split(param_table, by = split_cols)

  # Der Aufruf an den Worker wird vereinfacht.
  lgm_results_per_subset <- map(
    param_subsets,
    ~ .analyze_lgm_components(.x) # Kein `has_latent` mehr nötig
  )

  detect(lgm_results_per_subset, ~ .x$is_lgm)
}

#' @title Analyze LGM Components (Orchestrator)
#' @description Orchestrates the analysis of LGM components through a fail-fast
#'   pipeline. It calls a series of atomic helpers to identify all components
#'   of a Latent Growth Model in a clean, sequential workflow.
#' @param param_table The parameter data.table.
#' @return A list with LGM analysis results (`$is_lgm`, `$lgm_type`,
#'   `$element_groups`), or a list with `$is_lgm = FALSE` if not an LGM.
#' @keywords internal
#' @noRd
.analyze_lgm_components <- function(param_table) {
  loadings <- param_table[op == MODEL_OPS$LOADINGS, .(factor = lhs, indicator = rhs)]
  if (nrow(loadings) < 3) {
    return(list(is_lgm = FALSE))
  }

  regressions <- param_table[op == MODEL_OPS$REGRESSIONS, .(predictor = rhs, outcome = lhs)]

  all_latent_vars <- unique(loadings$factor)
  all_indicators <- unique(loadings$indicator)

  # --- Stage 2: Identification of Core LGM Components (Fail-Fast) ---
  timepoints <- .find_lgm_timepoints(loadings, all_latent_vars, all_indicators)
  if (is.null(timepoints)) {
    return(list(is_lgm = FALSE))
  }

  growth_factors <- .find_lgm_growth_factors(loadings, timepoints)
  if (is.null(growth_factors)) {
    return(list(is_lgm = FALSE))
  }

  # --- Stage 3: Identification of all other components ---
  latent_timepoints <- intersect(timepoints, all_latent_vars)
  measurement_occasions <- setdiff(all_indicators, all_latent_vars)
  predictors <- .find_lgm_predictors(regressions, growth_factors, timepoints)
  tv_covariates <- .find_lgm_tv_covariates(regressions, measurement_occasions)

  # --- Stage 4: Final Assembly ---
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
  by_factor <- intersect(all_indicators, all_latent_vars)

  timepoints <- if (length(by_factor) > 0) {
    by_factor
  } else {
    loadings[, .N, by = indicator][N > 1, indicator]
  }

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
  factors <- unique(loadings[indicator %in% timepoints, factor])

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
  if (length(latent_timepoints) == length(timepoints)) {
    "LGM_LATENT_MODEL"
  } else {
    "LGM_MANIFEST_MODEL"
  }
}


# ------------------------------------------------------------------------------
# Helpers for Defined Parameter Analysis
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

  # --- Guard Clauses for Base Cases (Atomic Paths) ---

  # Guard 1: The label is not a defined parameter or has no formula.
  if (nrow(row_data) == 0 || row_data$rhs == "") {
    return(path_label)
  }

  # Guard 2: The label refers to a simple, atomic regression or loading.
  is_atomic_path <- row_data$op %in% c(MODEL_OPS$REGRESSIONS, MODEL_OPS$LOADINGS)
  if (is_atomic_path) {
    return(row_data$label)
  }

  # Guard 3: The "definition" is not a complex := operation. Treat as atomic.
  if (row_data$op != MODEL_OPS$DEFINED) {
    return(path_label)
  }

  # --- Recursive Step (for complex defined parameters) ---

  # Split the formula string by any operator to get the components.
  parts <- strsplit(row_data$rhs, "[-+*()]") |> unlist()
  parts <- trimws(parts)
  parts <- parts[nzchar(parts)] # Use nzchar for a cleaner filter

  # Recursively call this function for each component.
  result_list <- lapply(parts, .resolve_to_path_labels, dt = dt)

  # Collect and return all unique atomic parts.
  return(unique(unlist(result_list)))
}


#' @#' @title Deconstruct Defined Paths and Enrich with Structure
#' @description This is the first core step in the defined path analysis pipeline.
#'   It takes the raw `defined_params` table, recursively deconstructs each
#'   parameter's formula into its constituent base path labels, and enriches
#'   this long-format table by joining the `from` and `to` node information for
#'   each base path.
#'
#' @param defined_params A `data.table` containing only the `:=` parameter rows.
#'   Must have a 'label' column.
#' @param direct_paths A lookup `data.table` mapping base path labels to their
#'   `from` and `to` nodes.
#' @param full_param_table The complete parameter table, required for the
#'   recursive lookup by `.resolve_to_path_labels`.
#'
#' @return A long-format `data.table` with one row per base path within each
#'   defined parameter. Columns include `defined_label`, `base_path_label`,
#'   `from`, and `to`.
#'
#' @keywords internal
#' @noRd
.deconstruct_defined_paths <- function(defined_params, direct_paths, full_param_table) {
  # Deconstruct each defined parameter's formula into a long list of base path labels
  deconstructed_long <- defined_params[, .(
    base_path_label = unlist(lapply(label, .resolve_to_path_labels, dt = full_param_table))
  ), by = .(defined_label = label)]

  # Join the `from` and `to` information for each base path.
  # This is a left join, keeping all deconstructed paths and adding structure info.
  # `nomatch = NULL` is used for an inner join to drop base paths that
  # don't have a corresponding direct path structure (e.g., numeric constants).
  paths_with_structure <- direct_paths[deconstructed_long, on = .(base_path_label), nomatch = NULL]

  return(paths_with_structure)
}

#' @title Aggregate Path Structure and Extract Mediators
#' @description This is the second core step in the pipeline. It takes the
#'   long-format, deconstructed path data and aggregates it back to a single
#'   row per defined parameter. During aggregation, it calculates the ultimate
#'   start (`from`) and end (`to`) nodes, extracts the list of all mediator
#'   nodes, and collects the chain of base path labels.
#'
#' @param paths_with_structure A long-format `data.table` from
#'   `.deconstruct_defined_paths`.
#'
#' @return A `data.table` with one row per `defined_label`, containing the
#'   aggregated structure: `from`, `to`, `mediators` (a list-column), and
#'   `base_paths` (a list-column).
#'
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
#' @description This is the final step in the pipeline. It takes the aggregated
#'   path structure information and joins it back to the original statistical
#'   and grouping data from the `defined_params` table. It also creates the
#'   final unique `id` for each defined path.
#'
#' @param path_structure_info The aggregated structure `data.table` from
#'   `.aggregate_path_structure`.
#' @param defined_params The original `:=` parameter rows, containing all
#'   statistical and grouping metadata.
#' @param expanded_value_columns A character vector of all value column names
#'   (e.g., "est.std", "est.unstd") to be kept.
#' @param group_cols A character vector of grouping column names (e.g., "group", "level").
#'
#' @return The final, complete, and clean `data.table` for all defined paths.
#'
#' @keywords internal
#' @noRd
.assemble_defined_paths <- function(path_structure_info, defined_params, expanded_value_columns, group_cols) {
  cols_to_keep <- intersect(
    c("label", "rhs", "sig", "edge_type", group_cols, expanded_value_columns),
    names(defined_params)
  )

  # Join the aggregated structure (i) to the original metadata (x).
  # This is a right join, ensuring all original defined parameters are present.
  final_data <- path_structure_info[defined_params[, ..cols_to_keep], on = .(defined_label = label)]

  # Clean up the final column names and create the unique ID.
  setnames(final_data, "defined_label", "label")
  final_data[, id := paste(substr(edge_type, 1, 3), .sanitize_string(label), sep = "_")]

  return(final_data)
}


#' @title Analyze Structure of Defined Paths (Orchestrator)
#' @description Orchestrates the analysis of user-defined parameters (`:=`) by
#'   passing the data through a clean, multi-phase pipeline that adheres to the
#'   Single Responsibility Principle. This function is the single entry point
#'   for this analysis and uses the pipe (`|>`) for a clear, readable data flow.
#'
#' @param param_table The full parameter data.table.
#' @param value_columns A character vector of BASE value column names to extract.
#'
#' @return A final data.table for all defined paths, containing their structure
#'   (from, to, mediators) and all associated statistical data.
#'
#' @keywords internal
#' @noRd
.analyze_defined_paths_structure <- function(param_table, value_columns = "est") {
  # --- 1. Preparation ---
  # Isolate defined parameters and perform initial classifications.
  defined_params <- param_table[op == MODEL_OPS$DEFINED]
  if (nrow(defined_params) == 0) {
    return(data.table::data.table())
  }

  setnames(defined_params, "lhs", "label")

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

  # Prepare arguments for the final assembly step.
  expanded_value_columns <- .expand_names_with_suffix(value_columns, c("std", "unstd"), ".")
  group_cols <- intersect(c("group", "level"), names(param_table))

  # --- 2. Main Pipeline ---
  # The pipeline now reads as a clear story:
  # "Take the defined parameters, deconstruct them, aggregate the structure,
  # and finally, assemble the final data."

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
# Helpers for Node Analysis
# ------------------------------------------------------------------------------

#' @title Expand Base Names with Suffixes
#' @description A general utility to create all combinations of base names and
#'   suffixes, joined by a separator.
#' @param base_names A character vector of base names.
#' @param suffixes A character vector of suffixes to append.
#' @param separator A character string to place between the base name and suffix.
#' @return A character vector of the fully expanded and unique names.
#' @keywords internal
#' @noRd
.expand_names_with_suffix <- function(base_names, suffixes, separator = ".") {
  # Return empty if either input is empty to avoid errors
  if (length(base_names) == 0 || length(suffixes) == 0) {
    return(character(0))
  }
  # Use outer product to create all combinations
  as.vector(outer(base_names, suffixes, paste, sep = separator))
}


# ==============================================================================
# Final Implementation: Node Analysis Phase
# ==============================================================================

#' @title Assemble the List of Nodes Using a Parameterized data.table Call
#' @description A safe, DRY-compliant factory that uses the `env` argument
#'   to safely and programmatically construct a node set from a source table.
#'   It interprets a declarative rule to filter rows and construct new columns.
#'
#' @param param_table The source `data.table` to operate on. For this package,
#'   this is the `primary_nodes` table created in the orchestrator function.
#' @param rule A named `list` that defines the node creation logic. It must
#'   contain the following elements:
#'   \describe{
#'     \item{`filter_expr`}{A `quote()`-d expression for filtering rows in `i`.}
#'     \item{`id_expr`}{A `quote()`-d expression for creating the unique `id` column.}
#'     \item{`unit_expr`}{A `quote()`-d expression for creating the `node_unit` column, which stores the original variable name.}
#'     \item{`node_type`}{A static character string defining the node type (from `NODE_TYPES`).}
#'   }
#' @param cols_to_keep A character vector of column names from `param_table`
#'   that should be preserved in the final output.
#'
#' @return A `data.table` containing the newly constructed nodes for the
#'   specified rule, including the preserved columns.
#'
#' @keywords internal
#' @noRd
.assemble_nodes_list <- function(param_table, rule, cols_to_keep) {
  # 1. Programmatically construct the `j` expression as a `call` object.
  #    This is "Computing on the Language" in a safe, structured way.
  #    It creates a `list(...)` call that defines the new columns and
  #    includes the original columns to be kept.
  j_call <- as.call(c(
    quote(list),
    list(
      id = rule$id_expr,
      node_unit = rule$unit_expr,
      node_type = rule$node_type,
      label = quote(if ("variable" %in% names(param_table)) as.character(variable) else ""),
      id_suffix = rule$id_suffix
    ),
    lapply(cols_to_keep, as.name)
  ))

  # 2. Execute the parameterized data.table call.
  #    This is the correct, idiomatic way to perform a fully dynamic query.
  #    The placeholders `.i` and `.j` are safely substituted by the `env`
  #    argument, avoiding `eval(parse(...))` and NSE pitfalls.
  param_table[
    .i,
    .j,
    env = list(
      .i = rule$filter_expr,
      .j = j_call
    )
  ]
}


#' @title Analyze and Extract All Node Structures (Orchestrator)
#' @description This function orchestrates the entire process of identifying and
#'   extracting all unique nodes from a `lavaan` parameter table. It follows a
#'   robust, multi-step process to ensure correctness and efficiency.
#'
#' @details
#' The process involves three main phases:
#' \enumerate{
#'   \item **Preparation:** A "master source table" (`primary_nodes`) is created
#'     by melting the `param_table`. This transforms the data into a long format
#'     where each row represents a single variable in a specific role (e.g.,
#'     'y1' as a 'Variance'). This is the single source of truth for all
#'     subsequent operations.
#'   \item **Rule-Based Extraction:** A list of declarative rules (`derived_node_rules`)
#'     defines how to identify and construct each type of node (latent, manifest,
#'     variance, etc.) from the master source table.
#'   \item **Assembly:** The factory function `.build_derived_nodes` is applied
#'     to each rule, generating a list of `data.table`s. These are then efficiently
#'     bound together into the final, complete node table.
#' }
#'
#' @param param_table A `data.table` of model parameters from a `lavaan` object.
#' @param value_columns A character vector of the base value column names (e.g., "est")
#'   to be expanded and included in the final node table.
#'
#' @return A final, clean `data.table` where each row represents a single,
#'   fully defined node with all its associated attributes (id, type, values, etc.).
#'
#' @keywords internal
#' @noRd
.analyze_node_structure <- function(param_table,
                                    value_columns = "est") {
  # --- 1. Preparation ---
  expanded_value_columns <- .expand_names_with_suffix(value_columns, c("std", "unstd"), ".")
  group_cols <- intersect(c("group", "level"), names(param_table))

  # --- 2. Create the Master Source Table (`primary_nodes`) ---
  # Melt the param_table to bring `lhs` and `rhs` into a single 'variable' column.
  # This robustly captures all variables, including exogenous ones.
  id_vars_for_melt <- names(param_table)[!names(param_table) %in% c("lhs", "rhs")]
  primary_vars_long <- data.table::melt(param_table, id.vars = id_vars_for_melt, measure.vars = c("lhs", "rhs"), variable.name = "source_col", value.name = "variable", na.rm = TRUE)[, variable := as.character(variable)]

  print(primary_vars_long)
  # Create the unique source table. Each row is a unique combination of a
  # variable and its role (`op`) within each group. This is the single
  # source of truth for the factory.
  primary_nodes <- primary_vars_long[nzchar(variable), unique(.SD, by = c("variable", "op", group_cols))]

  # Define which of the original columns should be carried through the process.
  cols_to_keep <- intersect(c("sig", group_cols, expanded_value_columns), names(primary_nodes))

  # --- 3. Apply Rules Using the Factory ---
  # Use lapply for a dependency-free, fast, and readable iteration.
  raw_nodes_list <- lapply(
    NODE_EXTRACTION_RULES,
    .assemble_nodes_list,
    param_table = primary_nodes,
    cols_to_keep = cols_to_keep
  )

  # --- 4. Assemble Final Node Table ---
  # Bind the list of processed data.tables into a single, final table.
  # No final `unique()` call is needed because the filtering logic in the
  # rules ensures that each node is created exactly once from a unique source row.
  all_nodes <- data.table::rbindlist(
    raw_nodes_list,
    use.names = TRUE, fill = TRUE
  )

  # Sanitize from/to columns after creation
  all_nodes[, id := .sanitize_string(id)]

  # --- 3. Transformation for Complex Structures (e.g., Moderation) ---
  all_nodes[!is.na(id_suffix) & nzchar(id_suffix), id := paste(id, id_suffix, sep = "_")]

  return(all_nodes[, !"id_suffix"])
}

# ------------------------------------------------------------------------------
# Helpers for Edge Analysis
# ------------------------------------------------------------------------------


#' @title Assemble a List of Edges Using a Parameterized Call
#' @description A safe, DRY-compliant factory that uses the `env` argument
#'   to programmatically construct an edge set from the `param_table`. It
#'   interprets a declarative rule to filter rows and construct the edge table.
#'
#' @param param_table The full `param_table` `data.table`.
#' @param rule A named `list` defining the edge creation logic from
#'   `EDGE_EXTRACTION_RULES`.
#' @param cols_to_keep A character vector of column names from `param_table`
#'   to preserve in the final output.
#'
#' @return A `data.table` containing the newly constructed edges for the
#'   specified rule.
#'
#' @keywords internal
#' @noRd
.assemble_edges_list <- function(param_table, rule, cols_to_keep) {
  # 1. Safely get the indices of rows that match the rule's filter.
  #    The `env` argument ensures the expression is evaluated in the
  #    context of `param_table` without NSE pitfalls.
  #    `.I` is a special `data.table` symbol that returns the row indices.
  matching_row_indices <- param_table[, .I[.i], env = list(.i = rule$filter_expr)]

  # 2. Guard Clause: If no rows match the filter, return an empty data.table
  #    immediately. This prevents the transformation logic from running on an
  #    empty set, which was the source of the "subscript out of bounds" error.
  if (length(matching_row_indices) == 0) {
    return(data.table::data.table())
  }

  # 3. Programmatically construct the `j` expression as a `call` object.
  #    This is identical to the original implementation.
  j_call <- as.call(c(
    quote(list),
    list(
      from = rule$from_expr,
      to = rule$to_expr,
      edge_type = rule$edge_type,
      id_prefix = rule$id_prefix,
      label = quote(if ("label" %in% names(param_table)) as.character(label) else "")
    ),
    lapply(cols_to_keep, as.name)
  ))

  # 4. Execute the transformation ONLY on the pre-filtered rows.
  #    We pass the calculated row indices to `i`, and the dynamic `j_call`
  #    to `.j` via the `env` argument.
  param_table[
    matching_row_indices,
    .j,
    env = list(
      .j = j_call
    )
  ]
}

#' @title Analyze and Extract All Edge Structures (Orchestrator)
#' @description This function orchestrates the extraction and transformation of all
#'   edges from a `lavaan` parameter table.
#'
#' @details
#' The process follows a clear, two-phase approach:
#' \enumerate{
#'   \item **Rule-Based Extraction:** It first iterates through the declarative
#'     `EDGE_EXTRACTION_RULES`, using the `.build_derived_edges` factory to
#'     extract all standard, raw edges (regressions, loadings, etc.) into a
#'     single table.
#'   \item **Transformation:** A subsequent call to a dedicated transformation
#'     function (e.g., for moderation) refines the raw edge table to represent
#'     more complex graph structures. This separation of concerns keeps the
#'     initial extraction clean and isolates complex logic.
#' }
#'
#' @param param_table A `data.table` of model parameters.
#' @param value_columns A character vector of base value column names to include.
#'
#' @return A final, clean `data.table` where each row represents a single,
#'   fully defined edge.
#'
#' @keywords internal
#' @noRd
.analyze_edge_structure <- function(param_table,
                                    value_columns = "est") {
  # --- 1. Preparation ---
  expanded_value_columns <- .expand_names_with_suffix(value_columns, c("std", "unstd"), ".")
  group_cols <- intersect(c("group", "level"), names(param_table))
  cols_to_keep <- intersect(c("sig", group_cols, expanded_value_columns), names(param_table))

  # --- 2. Rule-Based Extraction of All Raw Edges ---
  # Use lapply for a dependency-free, fast, and readable iteration.
  raw_edges_list <- lapply(
    EDGE_EXTRACTION_RULES,
    .assemble_edges_list,
    param_table = param_table,
    cols_to_keep = cols_to_keep
  )

  all_edges <- data.table::rbindlist(raw_edges_list, use.names = TRUE, fill = TRUE)

  # Final Guard Clause: If after all rules, no edges were created, return
  # the empty table before attempting to access columns that may not exist.
  if (nrow(all_edges) == 0) {
    return(all_edges)
  }

  # Sanitize from/to columns after creation
  all_edges[, from := .sanitize_string(from)][, to := .sanitize_string(to)]

  # --- 3. Transformation for Complex Structures (e.g., Moderation) ---
  # This step can be expanded later. For now, we create a unique ID.
  # The logic from the old `._modify_moderation_edges` would go here.
  all_edges[, id := paste(id_prefix, from, "to", to, sep = "_")]

  # --- 4. Finalization ---
  # Ensure uniqueness per edge and group.
  unique_cols <- intersect(c("id", group_cols), names(all_edges))
  all_edges <- unique(all_edges, by = unique_cols)

  return(all_edges)
}
