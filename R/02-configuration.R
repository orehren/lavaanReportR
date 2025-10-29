# ==============================================================================
# SECTION: CONFIGURATION HELPERS
# ==============================================================================

# --- Normalization Helpers ---

#' @title Normalize Raw User Arguments
#' @description Programmatically applies tolerant normalization using `match.arg`
#'   to a list of user arguments. It iterates through the rules defined in
#'   `NORMALIZATION_MAP` to handle partial matching for both single-choice and
#'   multiple-choice arguments.
#' @param user_args A named list of raw user arguments.
#' @return A named list of normalized user arguments. Invalid inputs are passed
#'   through unchanged for the validation phase to handle.
#' @keywords internal
#' @noRd
.normalize_user_args <- function(user_args) {
  # Iterate over the arguments defined in our normalization map.
  for (arg_name in names(NORMALIZATION_MAP)) {
    # Guard Clause: If the user has not provided this argument, skip to the next.
    if (is.null(user_args[[arg_name]])) {
      next
    }

    # Extract the rule and the user-provided value.
    rule <- NORMALIZATION_MAP[[arg_name]]
    arg_val <- user_args[[arg_name]]

    # Attempt to normalize the value using match.arg.
    # The tryCatch block ensures that any error from match.arg (e.g., invalid
    # choice, ambiguous partial match) is caught. In case of an error, we
    # intentionally do nothing, passing the original, invalid value to the
    # next stage (validation), which will then generate a user-friendly error.
    tryCatch(
      {
        user_args[[arg_name]] <- match.arg(
          arg = arg_val,
          choices = rule$choices,
          several.ok = rule$several.ok
        )
      },
      error = function(e) {}
    )
  }

  return(user_args)
}


#' @title Apply Default Values to User Arguments
#' @description Merges user-provided arguments over a complete list of defaults.
#'   Crucially, it ignores user-provided `NULL` values, ensuring that a
#'   type-stable default is always present.
#' @param user_args A named list of normalized user arguments.
#' @return A complete list of arguments with defaults applied.
#' @keywords internal
#' @noRd
.apply_user_arg_defaults <- function(user_args) {
  # `compact` removes all user-provided NULLs.
  # `utils::modifyList` then merges the non-NULL user arguments over the defaults.
  # This correctly implements the desired behavior: a user's NULL does NOT
  # overwrite a non-NULL default like `list()` or `character(0)`.
  utils::modifyList(DEFAULT_ARGS_MAP, compact(user_args))
}


# --- Validation Wrapper ---


#' @title Validate User Arguments
#' @description A wrapper that calls the main validation engine.
#' @param user_args A list of normalized and defaulted user arguments.
#' @param data_context The `param_table` for context-dependent checks.
#' @return The `user_args` list, passed through invisibly if validation succeeds.
#' @keywords internal
.validate_user_args <- function(user_args, data_context) {
  validation_call_args <- c(
    user_args,
    list(data_object = data_context, data_object_type = "sem_plot")
  )
  rlang::exec(.validate_function_args, !!!validation_call_args)
  return(user_args)
}


# --- Recipe Assembly Helpers ---


#' @title Build Base Recipe Component
#' @description Selects the correct default recipe from the global `recipe_list`
#'   based on the detected model type.
#' @param features A named list from `.analyze_model_features`, containing `$model_type`.
#' @return A list representing the base recipe.
#' @keywords internal
.build_base_recipe <- function(features) {
  # This function assumes a global `recipe_list` object exists.
  # We will need to create this object from `data-raw/plot_recipes.R`.
  recipe_list[[features$model_type]]
}


#' @title Build User Recipe Component
#' @description Safely extracts the user-provided recipe list from the user arguments.
#'   This represents the second layer of configuration.
#' @param user_args A list of user arguments.
#' @return The user's recipe list, or an empty list if not provided.
#' @keywords internal
#' @noRd
.build_user_recipe <- function(user_args) {
  user_args$recipe %||% list()
}


#' @title Build Direct Argument Overrides Component
#' @description Programmatically collects simple, direct argument overrides
#'   (e.g., `show_estimates`) into a list. These have the highest priority in the merge.
#' @param user_args A list of user arguments.
#' @return A list containing only the direct override values set by the user.
#' @keywords internal
#' @noRd
.build_direct_overrides <- function(user_args) {
  # This map defines which user argument corresponds to which recipe key.
  # It makes the function scalable and easy to maintain.
  arg_to_recipe_map <- c(
    "show_plot_elements" = "render_elements",
    "plot_flow_direction" = "rankdir",
    "show_estimates" = "show_estimates",
    "show_sig" = "show_sig",
    "show_estimates_above" = "show_estimates_above"
  )

  # Programmatically create the list of overrides from the map
  map(arg_to_recipe_map, ~ user_args[[.x]]) |>
    stats::setNames(names(arg_to_recipe_map)) |>
    compact() # `compact` removes NULLs, so only user-set arguments are returned
}


#' @title Build Fontsize Overrides Component
#' @description Interprets the `TEXT_SIZE_RULE_MAP` configuration table to transform
#'   the various `text_size_*` arguments into a `style_overrides` list.
#' @param user_args A list of user arguments.
#' @return A list containing `$style_overrides`, or `NULL` if no text size args were provided.
#' @keywords internal
#' @noRd
.build_fontsize_overrides <- function(user_args) {
  # --- 1. Identify which rules are TRULY active (i.e., not NA) ---

  # Get the subset of user arguments that are text size arguments
  text_size_args <- user_args[names(user_args) %in% TEXT_SIZE_RULE_MAP$arg_name]

  # Filter out any arguments that are NA. This is the core of the fix.
  # We only consider arguments that have a non-NA value.
  active_text_args <- Filter(function(x) !is.na(x), text_size_args)

  # Get the names of the arguments that were actually set by the user.
  active_arg_names <- names(active_text_args)

  # Guard Clause: Exit early if no non-NA arguments were provided.
  if (length(active_arg_names) == 0) {
    return(NULL)
  }

  # --- 2. Prepare and sort the active rules for processing ---

  # Filter the rule map to get only the active rules.
  active_rules <- TEXT_SIZE_RULE_MAP[arg_name %in% active_arg_names]

  # Sort the rules by priority to ensure correct override behavior.
  data.table::setorderv(active_rules, "priority")

  # --- 3. Apply the rules sequentially to build the final font size list ---

  # Convert the rules table into a list of rows for efficient iteration.
  rules_list <- split(active_rules, f = seq_len(nrow(active_rules)))

  # Use reduce to apply each rule in sequence.
  font_sizes <- reduce(
    .x = rules_list,
    .f = ~ .apply_text_size_rule(.x, .y, user_args),
    .init = list()
  )

  # --- 4. Format the final output ---

  # Wrap the flat list of font sizes into the nested list structure
  # required for the `style_overrides` recipe key.
  list(style_overrides = map(font_sizes, ~ list(fontsize = .x)))
}


#' @title Build Model Hierarchy Component
#' @description Determines the split hierarchy based on model features and user arguments.
#' @param features A list of detected model features.
#' @param user_args A list of user arguments.
#' @return A list containing only the `$split_hierarchy`.
#' @keywords internal
.build_model_hierarchy <- function(features, user_args) {
  # Use the clean fcase logic to determine the hierarchy
  split_hierarchy <- data.table::fcase(
    features$has_group && features$has_level, list(user_args$multilevel_multigroup_order),
    features$has_group, list("group"),
    features$has_level, list("level"),
    default = list(character(0))
  )

  list(
    split_hierarchy = unlist(split_hierarchy)
  )
}


#' @title Merge Multiple Recipe Lists
#' @description Merges a series of lists hierarchically using `utils::modifyList`.
#'   Each subsequent list in the sequence overrides the values of the previous ones.
#'   `NULL` elements in the input list are ignored.
#' @param ... A sequence of lists to merge.
#' @return A single, merged list.
#' @keywords internal
#' @noRd
.merge_recipes <- function(...) {
  # `compact` removes any NULL lists (e.g., if fontsize_overrides is NULL)
  # `reduce` then iteratively applies `modifyList` to the sequence.
  reduce(compact(list(...)), utils::modifyList)
}


#' @title Assemble the Recipe (Orchestrator)
#' @description Orchestrates the creation of the final recipe by building and
#'   merging all necessary components in the correct priority order. This function
#'   represents the core of the configuration merging logic.
#' @param features A list of detected model features.
#' @param user_args A list of normalized and validated user arguments.
#' @return The final, complete recipe list.
#' @keywords internal
#' @noRd
.assemble_recipe <- function(features, user_args) {
  # --- 1. Build all individual recipe components ---
  # Each of these functions returns a list (a "partial recipe").

  base_recipe <- .build_base_recipe(features)
  user_recipe <- .build_user_recipe(user_args)
  direct_overrides <- .build_direct_overrides(user_args)
  fontsize_overrides <- .build_fontsize_overrides(user_args)
  model_hierarchy <- .build_model_hierarchy(features, user_args)

  # --- 2. Perform the single, final merge in the correct priority order ---
  # The `merge_recipes` function handles NULL inputs gracefully.
  # The order determines the hierarchy: later lists override earlier ones.

  .merge_recipes(
    base_recipe,
    user_recipe,
    direct_overrides,
    fontsize_overrides,
    model_hierarchy
  )
}


.sanitize_param_table <- function(param_table) {
  # Columns that should always be character for our operations
  char_cols <- c("lhs", "op", "rhs", "label", "group", "level")

  # Find which of these columns actually exist in the table
  cols_to_sanitize <- intersect(char_cols, names(param_table))

  # Use the robust data.table way to convert multiple columns to character
  if (length(cols_to_sanitize) > 0) {
    param_table[, (cols_to_sanitize) := lapply(.SD, as.character), .SDcols = cols_to_sanitize]
  }

  return(param_table)
}


#' @title Apply a Single Text Size Rule
#' @description An atomic helper for `purrr::reduce`. It takes the current font
#'   size list and applies a single transformation rule to it.
#' @param current_sizes The accumulating list of font sizes.
#' @param rule A **vector** representing a row from `TEXT_SIZE_RULE_MAP`. The
#'   elements correspond to the columns: `priority`, `arg_name`, `target_keys`.
#' @param user_args The full list of user arguments.
#' @return The updated list of font sizes.
#' @keywords internal
#' @noRd
.apply_text_size_rule <- function(current_sizes, rule, user_args) {
  # The `rule` is now a simple vector, not a data.table.
  # We access its elements by index, corresponding to the column order in
  # TEXT_SIZE_RULE_MAP: 1=priority, 2=arg_name, 3=target_keys
  arg_name <- rule$arg_name
  arg_val <- user_args[[arg_name]]
  target_keys <- rule$target_keys[[1]]

  update_list <- if (is.null(target_keys)) {
    arg_val
  } else {
    values <- as.list(rep(arg_val, length(target_keys)))
    stats::setNames(values, target_keys)
  }

  utils::modifyList(current_sizes, update_list)
}
