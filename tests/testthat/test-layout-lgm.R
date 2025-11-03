# Load necessary libraries
library(devtools)
library(testthat)
library(data.table)
library(purrr)

# Load internal data and source files
if (file.exists("../../R/sysdata.rda")) {
  load("../../R/sysdata.rda")
  lapply(list.files("../../R", pattern = "\\.R$", full.names = TRUE), source)
} else {
  load_all()
}

# ==============================================================================
# TEST FIXTURE: LGM with Time-Varying Covariates
# ==============================================================================
FIXTURE_MODEL_LGM_TVC <- '
  # Growth factors
  i =~ 1*x1 + 1*x2 + 1*x3 + 1*x4
  s =~ 0*x1 + 1*x2 + 2*x3 + 3*x4

  # Time-varying covariates
  x1 ~ x5
  x2 ~ x6
  x3 ~ x7
  x4 ~ x8

  # Time-invariant covariates
  i ~ x9 + id
  s ~ x9 + id
'
FIXTURE_DATA_LGM_TVC <- lavaan::HolzingerSwineford1939

FIXTURE_PTABLE_LGM_TVC <- lavaan::growth(FIXTURE_MODEL_LGM_TVC, data = FIXTURE_DATA_LGM_TVC) |>
  extract_model_estimates_data()

FIXTURE_ANALYZED_LGM_TVC <- analyze(FIXTURE_PTABLE_LGM_TVC)


# ==============================================================================
# LGM LAYOUT TESTS
# ==============================================================================

test_that("Automated LGM layout correctly ranks all covariates together", {
  # --- 1. Default Layout ---
  config_default <- configure_plot(FIXTURE_ANALYZED_LGM_TVC)
  layout_default_obj <- layout(config_default)

  # Extract the rank data into a convenient format
  ranks_default <- .analyze_layout_structure(
    layout_default_obj$config$analyzed_model$nodes,
    layout_default_obj$config$analyzed_model$edges,
    layout_default_obj$config$analyzed_model$features$element_groups
  )$levels

  rank_dt_default <- data.table(
    id = unlist(ranks_default, use.names = FALSE),
    rank = as.numeric(rep(names(ranks_default), lengths(ranks_default)))
  )

  # --- 2. Verification ---
  # All time-varying and time-invariant covariates should now be in the same, single rank.
  rank_of_x9 <- rank_dt_default[id == "x9", rank]

  expect_equal(rank_dt_default[id == "x5", rank], rank_of_x9)
  expect_equal(rank_dt_default[id == "x6", rank], rank_of_x9)
  expect_equal(rank_dt_default[id == "x7", rank], rank_of_x9)
  expect_equal(rank_dt_default[id == "x8", rank], rank_of_x9)
})

test_that("Manual rank override still works correctly on LGM layouts", {
  # --- 1. Manual Rank Override Layout ---
  # We will force the tv_covariates to a completely different rank
  manual_override_rank <- 4

  config_manual <- configure_plot(
    FIXTURE_ANALYZED_LGM_TVC,
    manual_ranks = list(tv_covariates = manual_override_rank)
  )

  layout_manual_obj <- layout(config_manual)

  ranks_manual <- .analyze_layout_structure(
    layout_manual_obj$config$analyzed_model$nodes,
    layout_manual_obj$config$analyzed_model$edges,
    layout_manual_obj$config$analyzed_model$features$element_groups,
    manual_ranks = list(tv_covariates = manual_override_rank)
  )$levels

  rank_dt_manual <- data.table(
    id = unlist(ranks_manual, use.names = FALSE),
    rank = as.numeric(rep(names(ranks_manual), lengths(ranks_manual)))
  )

  # --- 2. Verification ---
  # All tv_covariates should be in the manually specified rank.
  expect_equal(rank_dt_manual[id == "x5", rank], manual_override_rank)
  expect_equal(rank_dt_manual[id == "x6", rank], manual_override_rank)
  expect_equal(rank_dt_manual[id == "x7", rank], manual_override_rank)
  expect_equal(rank_dt_manual[id == "x8", rank], manual_override_rank)

  # The rank of the time-invariant covariate should NOT have changed.
  # We get its original rank from the default layout test.
  config_default <- configure_plot(FIXTURE_ANALYZED_LGM_TVC)
  layout_default_obj <- layout(config_default)
  ranks_default <- .analyze_layout_structure(layout_default_obj$config$analyzed_model$nodes, layout_default_obj$config$analyzed_model$edges, layout_default_obj$config$analyzed_model$features$element_groups)$levels
  rank_dt_default <- data.table(id = unlist(ranks_default, use.names = FALSE), rank = as.numeric(rep(names(ranks_default), lengths(ranks_default))))
  original_x9_rank <- rank_dt_default[id == "x9", rank]

  expect_equal(rank_dt_manual[id == "x9", rank], original_x9_rank)
})
