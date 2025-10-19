validation_rules <- list(
  # --- Plotting Arguments (plot_sem_path) ---
  estimates_to_show = list( # NEW
    validator_fn = .assert_argument_is_plausible_string_selector,
    args = list(allowed_strings = c("std", "unstd", "standardized", "unstandardized")),
    is_quo = TRUE
  ),
  recipe = list(
    validator_fn = .assert_argument_is_class,
    args = list(expected_class = "list", allow_null = TRUE)
  ),
  show_plot_elements = list( # UPDATED
    validator_fn = .assert_argument_is_plausible_string_selector,
    args = list(allowed_strings = c("intercepts", "variances", "covariances"), allow_null = TRUE),
    is_quo = TRUE
  ),
  plot_flow_direction = list(
    validator_fn = .assert_argument_is_plausible_string_selector,
    args = list(allowed_strings = c("TB", "LR"), allow_null = TRUE),
    is_quo = TRUE
  ),
  show_sig = list(
    validator_fn = .assert_argument_is_flag,
    args = list(allow_null = TRUE)
  ),
  text_size_global = list(
    validator_fn = .assert_argument_is_single_numeric,
    args = list(allow_null = TRUE, allow_na = TRUE, allow_negative_value = FALSE)
  ),
  text_size_nodes = list(
    validator_fn = .assert_argument_is_single_numeric,
    args = list(allow_null = TRUE, allow_na = TRUE, allow_negative_value = FALSE)
  ),
  text_size_edges = list(
    validator_fn = .assert_argument_is_single_numeric,
    args = list(allow_null = TRUE, allow_na = TRUE, allow_negative_value = FALSE)
  ),
  text_size_details = list(
    validator_fn = .assert_argument_is_named_list,
    args = list(allow_null = TRUE)
  ),
  show_estimates_above = list(
    validator_fn = .assert_argument_is_single_numeric,
    # Hier sind negative Werte erlaubt (obwohl sie selten Sinn machen)
    args = list(allow_null = TRUE, allow_na = FALSE, allow_negative_value = TRUE)
  ),
  show_legend = list(
    validator_fn = .assert_argument_is_flag,
    args = list(allow_null = TRUE)
  ),
  show_legend_with_effects = list(
    validator_fn = .assert_argument_is_flag,
    args = list(allow_null = TRUE)
  ),
  multilevel_multigroup_order = list(
    validator_fn = .assert_vector_contains_all_expected_strings,
    args = list(expected_strings = c("group", "level"), allow_null = TRUE)
  ),
  node_labels = list(
    validator_fn = .assert_names_are_in_allowed_set,
    is_contextual = TRUE
  ),
  effect_labels = list(
    validator_fn = .assert_names_are_in_allowed_set,
    is_contextual = TRUE
  ),
  render = list(
    validator_fn = .assert_argument_is_flag,
    args = list(allow_null = TRUE)
  )
)
