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

test_that("Automated LGM layout produces the correct 4-level hierarchy", {
  # --- 1. Setup ---
  config <- configure_plot(FIXTURE_ANALYZED_LGM_TVC)
  layout_obj <- layout(config)

  ranks <- .analyze_layout_structure(
    layout_obj$config$analyzed_model$nodes,
    layout_obj$config$analyzed_model$edges,
    layout_obj$config$analyzed_model$features$element_groups
  )$levels

  rank_dt <- data.table(
    id = unlist(ranks, use.names = FALSE),
    rank = as.numeric(rep(names(ranks), lengths(ranks)))
  )

  # Helper to check if all nodes of a group are in a specific rank
  expect_nodes_in_rank <- function(node_ids, target_rank) {
    for (node_id in node_ids) {
      expect_equal(rank_dt[id == node_id, rank], target_rank,
                   info = paste("Node", node_id, "failed rank check."))
    }
  }

  # --- 2. Verification ---
  element_groups <- FIXTURE_ANALYZED_LGM_TVC$features$element_groups

  expect_nodes_in_rank(element_groups$predictors, 1)
  expect_nodes_in_rank(element_groups$growth_factors, 2)
  expect_nodes_in_rank(element_groups$measurement_occasions, 3)
  expect_nodes_in_rank(element_groups$tv_covariates, 4)
})

test_that("Manual rank override still works on the automated LGM layout", {
  # --- 1. Setup ---
  # Swap the ranks of predictors and TVCs
  manual_ranks_override <- list(
    predictors = 4,
    tv_covariates = 1
  )

  config <- configure_plot(FIXTURE_ANALYZED_LGM_TVC, manual_ranks = manual_ranks_override)
  layout_obj <- layout(config)

  ranks <- .analyze_layout_structure(
    layout_obj$config$analyzed_model$nodes,
    layout_obj$config$analyzed_model$edges,
    layout_obj$config$analyzed_model$features$element_groups,
    manual_ranks = manual_ranks_override
  )$levels

  rank_dt <- data.table(
    id = unlist(ranks, use.names = FALSE),
    rank = as.numeric(rep(names(ranks), lengths(ranks)))
  )

  # Helper function
  expect_nodes_in_rank <- function(node_ids, target_rank) {
    for (node_id in node_ids) {
      expect_equal(rank_dt[id == node_id, rank], target_rank,
                   info = paste("Node", node_id, "failed rank check."))
    }
  }

  # --- 2. Verification ---
  element_groups <- FIXTURE_ANALYZED_LGM_TVC$features$element_groups

  # Check the overridden ranks
  expect_nodes_in_rank(element_groups$predictors, 4)
  expect_nodes_in_rank(element_groups$tv_covariates, 1)

  # Check that the other ranks are untouched
  expect_nodes_in_rank(element_groups$growth_factors, 2)
  expect_nodes_in_rank(element_groups$measurement_occasions, 3)
})
