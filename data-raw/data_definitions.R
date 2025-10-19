# data-raw/internal_object_definitions.R

## code to prepare `internal_object_definitions` dataset goes here

# --- 0. source validation rule book ---
source(here::here("data-raw", "validation_rules.R"))
#
# # --- 1. general definitions ---
# p_value_explanation <- "'*' p < .05, '**' p < .01, '***' p < .001"
#
# # --- 2. source footnotes ---
# # footnote_templates
# source(here::here("data-raw", "config_footnotes.R"))
#
# # --- 3. definitions for data.table functions ---
# # fit_sections_definitions
# source(here::here("data-raw", "definitions_model_fit.R"))
# # info_definitions
# source(here::here("data-raw", "definitions_model_info.R"))
#
#
# # --- 4. source gt table configurations ---
# # config_template_reliability
# source(here::here("data-raw", "config_template_reliability.R"))
#
# # config_template_model_info
# source(here::here("data-raw", "config_template_model_info.R"))
#
# # config_template_model_fit
# source(here::here("data-raw", "config_template_model_fit.R"))
#
# # config_template_model_estimates
# source(here::here("data-raw", "config_template_model_estimates.R"))
#
# # config_template_model_comparison
# source(here::here("data-raw", "config_template_model_comparison.R"))

# plot_recipes
source(here::here("data-raw", "plot_recipes.R"))

# --- 5. create package data base (sysdat.rda) ---
usethis::use_data(
  validation_rules,
  # p_value_explanation,
  # footnote_templates,
  # fit_sections_definitions,
  # info_definitions,
  # config_template_reliability,
  # config_template_model_info,
  # config_template_model_fit,
  # config_template_model_estimates,
  # config_template_model_comparison,
  recipe_list,
  internal = TRUE,
  overwrite = TRUE
)
