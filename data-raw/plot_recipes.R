# ==============================================================================
# DATA-RAW SCRIPT: Build Internal Recipe List
# ==============================================================================
# This script defines the `recipe_list` object, which contains the default
# plotting configurations for different model types.
# To run this script and update the data object, use:
# source("data-raw/plot_recipes.R")
# ==============================================================================

# --- 1. Define the Default Recipe ---
# This list contains all common settings shared across most model types.
default_recipe <- list(
  # Note: The layout function is now a string/expression that will be
  # evaluated later. We refer to the new, clean function name.
  layout_function = rlang::expr(.calculate_layout),
  rankdir = "TB",
  unit_layout = list(
    manifest = list(intercept_edge = c("intercept", "variable"), variance_edge = c("variable", "variance")),
    latent = list(intercept_edge = c("intercept", "variable"), variance_edge = c("variable", "variance"))
  ),
  hierarchy_edge_attrs = list(style = "invis", constraint = FALSE),

  # Default elements to render
  render_elements = c("intercepts", "variances", "covariances"),

  # Default settings for user-configurable arguments
  show_estimates = TRUE,
  show_sig = TRUE,
  show_estimates_above = 0,

  # Placeholder for style overrides
  style_overrides = list()
)

# --- 2. Define Model-Specific Overrides ---
# These lists only contain settings that *differ* from the default_recipe.

path_model_overrides <- list(
  rankdir = "LR",
  rank_order = c("all_vars")
)

cfa_model_overrides <- list(
  rank_order = c("all_vars")
)

lgm_manifest_overrides <- list(
  # The LGM parser is a model-specific analysis step
  parser_function = rlang::expr(.analyze_lgm_structure),
  rank_order = c("predictors", "growth_factors", "measurement_occasions", "tv_covariates"),
  render_elements = c("intercepts", "variances"),
  hierarchy_edge_attrs = list(style = "invis", constraint = TRUE)
)

lgm_latent_overrides <- list(
  parser_function = rlang::expr(.analyze_lgm_structure),
  rank_order = c("predictors", "growth_factors", "timepoint_factors", "measurement_occasions", "tv_covariates"),
  render_elements = c("intercepts", "variances"),
  hierarchy_edge_attrs = list(style = "invis", constraint = TRUE)
)

# --- 3. Construct and Save the Final Recipe List ---
recipe_list <- list(
  "PATH_MODEL" = utils::modifyList(default_recipe, path_model_overrides),
  "CFA_MODEL" = utils::modifyList(default_recipe, cfa_model_overrides),
  "LGM_MANIFEST_MODEL" = utils::modifyList(default_recipe, lgm_manifest_overrides),
  "LGM_LATENT_MODEL" = utils::modifyList(default_recipe, lgm_latent_overrides)
)
