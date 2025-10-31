# Load necessary libraries
library(devtools)
library(testthat)
library(data.table)
library(purrr)

# Load internal data and source files
load("../../R/sysdata.rda")
lapply(list.files("../../R", pattern = "\\.R$", full.names = TRUE), source)

test_that("Grid-based layout algorithm calculates coordinates correctly", {
  # 1. Define the example model and run the pipeline to get the layout
  sp_model <- '
    y5 ~ y4
    y4 ~ y2 + y3
    y2 ~ y1
  '
  # The test requires a full parameter table. The default call works perfectly.
  model_ptable <- lavaan::sem(sp_model, data = lavaan::PoliticalDemocracy, meanstructure = TRUE) |>
    extract_model_estimates_data()

  # Correctly extract the layout data.table
  layout_obj <- model_ptable |>
    analyze() |>
    configure_plot() |>
    layout()

  layout_dt <- layout_obj$layout

  # Merge rank information back in for testing purposes
  ranks <- .analyze_layout_structure(layout_obj$config$analyzed_model$nodes, layout_obj$config$analyzed_model$edges)
  rank_dt <- data.table(
    id = unlist(ranks$levels, use.names = FALSE),
    rank = as.numeric(rep(names(ranks$levels), lengths(ranks$levels)))
  )
  layout_dt[rank_dt, on = "id", rank := i.rank]


  # 2. Define expected properties
  expected_ranks <- 4
  median_rank <- (1 + expected_ranks) / 2
  section_size <- 3

  # 3. Assertions for Primary Axis (x-coordinates for LR flow)
  # Check that each rank is in the correct section
  expect_equal(unique(layout_dt[rank == 1, x]), (1 - median_rank) * section_size) # -4.5
  expect_equal(unique(layout_dt[rank == 2, x]), (2 - median_rank) * section_size) # -1.5
  expect_equal(unique(layout_dt[rank == 3, x]), (3 - median_rank) * section_size) #  1.5
  expect_equal(unique(layout_dt[rank == 4, x]), (4 - median_rank) * section_size) #  4.5

  # 4. Assertions for Satellite Node Placement
  # Check that satellites are correctly offset from their main nodes
  y1_pos <- layout_dt[id == "y1", y]
  y1_var_pos <- layout_dt[id == "y1_var", y]
  y1_int_pos <- layout_dt[id == "y1_int", y]

  expect_equal(y1_var_pos, y1_pos + 0.5)
  expect_equal(y1_int_pos, y1_pos - 0.5)

  y2_pos <- layout_dt[id == "y2", y]
  y2_var_pos <- layout_dt[id == "y2_var", y]
  y2_int_pos <- layout_dt[id == "y2_int", y]

  expect_equal(y2_var_pos, y2_pos + 0.5)
  expect_equal(y2_int_pos, y2_pos - 0.5)
})
