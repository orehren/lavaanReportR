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
  # The new logic gives satellites a slight primary-axis offset.
  # Therefore, we only test the main nodes for rank centering.
  main_nodes_layout <- layout_dt[id %in% c("y1", "y2", "y3", "y4", "y5")]
  expect_equal(unique(main_nodes_layout[rank == 1, x]), (1 - median_rank) * section_size) # -4.5
  expect_equal(unique(main_nodes_layout[rank == 2, x]), (2 - median_rank) * section_size) # -1.5
  expect_equal(unique(main_nodes_layout[rank == 3, x]), (3 - median_rank) * section_size) #  1.5
  expect_equal(unique(main_nodes_layout[rank == 4, x]), (4 - median_rank) * section_size) #  4.5

  # 4. Assertions for New "Look-Ahead" Satellite Placement

  # Case 1: y1 is a predictor. Its child is y2.
  # Satellites should be placed on the opposite side of y2.
  y1_coords <- layout_dt[id == "y1"]
  y2_coords <- layout_dt[id == "y2"]
  y1_var_coords <- layout_dt[id == "y1_var"]
  y1_int_coords <- layout_dt[id == "y1_int"]

  # The test should not assume the relative position of y1 and y2, which
  # depends on the barycenter sort. Instead, it must test that the satellites
  # are placed on the correct *opposite* side, whichever that may be.
  if (y2_coords$y > y1_coords$y) {
    # y2 is above y1, so satellites should be below.
    expect_equal(y1_var_coords$y, y1_coords$y - 0.5)
    expect_equal(y1_int_coords$y, y1_coords$y - 0.5)
  } else {
    # y2 is below y1, so satellites should be above.
    expect_equal(y1_var_coords$y, y1_coords$y + 0.5)
    expect_equal(y1_int_coords$y, y1_coords$y + 0.5)
  }

  # We can always expect the side-by-side placement on the x-axis.
  expect_equal(y1_var_coords$x, y1_coords$x - 0.25)
  expect_equal(y1_int_coords$x, y1_coords$x + 0.25)

  # Case 2: y5 is an outcome-only node.
  # Satellites should be placed in the direction of graph flow (to the right).
  y5_coords <- layout_dt[id == "y5"]
  y5_var_coords <- layout_dt[id == "y5_var"]
  y5_int_coords <- layout_dt[id == "y5_int"]

  expect_equal(y5_var_coords$x, y5_coords$x + 0.5)
  expect_equal(y5_int_coords$x, y5_coords$x + 0.5)
  # Check side-by-side placement on the y-axis
  expect_equal(y5_var_coords$y, y5_coords$y - 0.25)
  expect_equal(y5_int_coords$y, y5_coords$y + 0.25)

  # Case 3: An isolated node (y_iso)
  # This requires a separate model run.
  iso_model <- ' y_iso ~~ y_iso '
  iso_ptable <- lavaan::sem(iso_model, data = data.frame(y_iso = rnorm(100)), meanstructure = TRUE) |>
    extract_model_estimates_data()

  iso_layout_dt <- iso_ptable |>
    analyze() |>
    configure_plot() |>
    layout() |>
    getElement("layout")

  y_iso_coords <- iso_layout_dt[id == "y_iso"]
  y_iso_var_coords <- iso_layout_dt[id == "y_iso_var"]
  y_iso_int_coords <- iso_layout_dt[id == "y_iso_int"]

  # Satellites should be placed on opposite sides of the secondary axis (y).
  expect_equal(y_iso_var_coords$y, y_iso_coords$y + 0.5)
  expect_equal(y_iso_int_coords$y, y_iso_coords$y - 0.5)
  # And they should have a side-by-side spread on the primary axis (x).
  expect_equal(y_iso_var_coords$x, y_iso_coords$x - 0.25)
  expect_equal(y_iso_int_coords$x, y_iso_coords$x + 0.25)
})
