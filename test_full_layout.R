# Load necessary libraries
library(devtools)
library(testthat)
library(data.table)
library(purrr)

# Load internal data
load("R/sysdata.rda")

# Source all R files
lapply(list.files("R", pattern = "\\.R$", full.names = TRUE), source)
source("R/table_estimates_data.R")

# Define the test
test_that("Moderation model layout is calculated correctly", {
  # fit the most simplistic moderation model and create parameter table
  moderation_model_ptable <- lavaan::sem(
    'y4 ~ y1 + y1:y4',
    data = lavaan::PoliticalDemocracy,
    meanstructure = TRUE,
    parallel = "multicore",
    ncpus = parallel::detectCores() - 1
  ) |> extract_model_estimates_data()

  # create the corresponding plot analysis
  moderation_model_analyzed <- moderation_model_ptable |> analyze()

  # create the corresponding plot configuration
  moderation_model_configuration <- moderation_model_analyzed |> configure_plot()

  # calculate the corresponding plot layout
  moderation_model_layout <- moderation_model_configuration |> layout()

  # Part A: Verify the hierarchical ranks
  ranks <- .analyze_layout_structure(moderation_model_analyzed$nodes, moderation_model_analyzed$edges)
  expected_ranks <- list(
    `1` = c("y1", "y1_var", "y1_int"),
    `2` = c("y1_y4", "y1_y4_path", "y1_y4_var", "y1_y4_int"),
    `3` = c("y4", "y4_var", "y4_int")
  )
  expect_equal(lapply(ranks$levels, sort), lapply(expected_ranks, sort))

  # Part B: Verify coordinate properties
  layout_dt <- moderation_model_layout$layout

  # 1. Check if the correct number of nodes are present
  expect_equal(nrow(layout_dt), 10)

  # 2. Check that for each y-level, all x-coordinates are unique
  expect_true(all(layout_dt[, .N, by = .(y, x)]$N == 1))
})
