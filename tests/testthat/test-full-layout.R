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
# TEST FIXTURE: A standard, multi-rank model
# ==============================================================================
FIXTURE_MODEL_STD <- '
  y5 ~ y4
  y4 ~ y2 + y3
  y2 ~ y1
'
FIXTURE_PTABLE_STD <- lavaan::sem(FIXTURE_MODEL_STD, data = lavaan::PoliticalDemocracy, meanstructure = TRUE) |>
  extract_model_estimates_data()
FIXTURE_ANALYZED_STD <- analyze(FIXTURE_PTABLE_STD)

# ==============================================================================
# TEST FIXTURE: An isolated node model
# ==============================================================================
FIXTURE_MODEL_ISO <- ' y_iso ~~ y_iso '
FIXTURE_PTABLE_ISO <- lavaan::sem(FIXTURE_MODEL_ISO, data = data.frame(y_iso = rnorm(100)), meanstructure = TRUE) |>
  extract_model_estimates_data()
FIXTURE_ANALYZED_ISO <- analyze(FIXTURE_PTABLE_ISO)

# ==============================================================================
# TEST FIXTURE: A moderation model
# ==============================================================================
FIXTURE_MODEL_MOD <- ' y3 ~ y1 + y2 + y1:y2 '
FIXTURE_DATA_MOD <- data.frame(y1 = rnorm(100), y2 = rnorm(100), y3 = rnorm(100))
FIXTURE_PTABLE_MOD <- lavaan::sem(FIXTURE_MODEL_MOD, data = FIXTURE_DATA_MOD, meanstructure = TRUE) |>
  extract_model_estimates_data()
FIXTURE_ANALYZED_MOD <- analyze(FIXTURE_PTABLE_MOD)


# ==============================================================================
# CORE LAYOUT TESTS
# ==============================================================================

test_that("Primary axis coordinates are correct for LR and TB layouts", {
  # --- LR Layout ---
  config_lr <- configure_plot(FIXTURE_ANALYZED_STD, recipe = list(rankdir = "LR"))
  layout_lr <- layout(config_lr) |> getElement("layout")

  ranks <- .analyze_layout_structure(FIXTURE_ANALYZED_STD$nodes, FIXTURE_ANALYZED_STD$edges)$levels
  rank_dt <- data.table(id = unlist(ranks, use.names = FALSE), rank = as.numeric(rep(names(ranks), lengths(ranks))))
  layout_lr[rank_dt, on = "id", rank := i.rank]

  n_ranks <- 4
  median_rank <- (1 + n_ranks) / 2
  section_size <- 3

  main_nodes_lr <- layout_lr[id %in% c("y1", "y2", "y3", "y4", "y5")]
  expect_equal(unique(main_nodes_lr[rank == 1, x]), (1 - median_rank) * section_size)
  expect_equal(unique(main_nodes_lr[rank == 2, x]), (2 - median_rank) * section_size)
  expect_equal(unique(main_nodes_lr[rank == 3, x]), (3 - median_rank) * section_size)
  expect_equal(unique(main_nodes_lr[rank == 4, x]), (4 - median_rank) * section_size)

  # --- TB Layout ---
  config_tb <- configure_plot(FIXTURE_ANALYZED_STD, recipe = list(rankdir = "TB"))
  layout_tb <- layout(config_tb) |> getElement("layout")
  layout_tb[rank_dt, on = "id", rank := i.rank]

  main_nodes_tb <- layout_tb[id %in% c("y1", "y2", "y3", "y4", "y5")]
  expect_equal(unique(main_nodes_tb[rank == 1, y]), -(1 - median_rank) * section_size)
  expect_equal(unique(main_nodes_tb[rank == 2, y]), -(2 - median_rank) * section_size)
  expect_equal(unique(main_nodes_tb[rank == 3, y]), -(3 - median_rank) * section_size)
  expect_equal(unique(main_nodes_tb[rank == 4, y]), -(4 - median_rank) * section_size)
})


# ==============================================================================
# SATELLITE PLACEMENT TESTS
# ==============================================================================

test_that("Satellite placement for PREDICTOR nodes is correct (look-ahead)", {
  config <- configure_plot(FIXTURE_ANALYZED_STD, recipe = list(rankdir = "LR"))
  layout_dt <- layout(config) |> getElement("layout")

  y1_coords <- layout_dt[id == "y1"]
  y2_coords <- layout_dt[id == "y2"]
  y1_var_coords <- layout_dt[id == "y1_var"]
  y1_int_coords <- layout_dt[id == "y1_int"]

  y_direction <- sign(y1_coords$y - y2_coords$y)

  expect_equal(y1_var_coords$y, y1_coords$y + y_direction * 0.5)
  expect_equal(y1_int_coords$y, y1_coords$y + y_direction * 0.5)

  expect_equal(y1_var_coords$x, y1_coords$x - 0.25)
  expect_equal(y1_int_coords$x, y1_coords$x + 0.25)
})

test_that("Satellite placement for OUTCOME-ONLY & MODERATOR is correct (flow-direction)", {
  # --- OUTCOME-ONLY NODE ---
  config_std <- configure_plot(FIXTURE_ANALYZED_STD, recipe = list(rankdir = "LR"))
  layout_std <- layout(config_std) |> getElement("layout")

  y5_coords <- layout_std[id == "y5"]
  y5_var_coords <- layout_std[id == "y5_var"]
  y5_int_coords <- layout_std[id == "y5_int"]

  # Primary axis is 'x'. Satellites offset on 'x'.
  expect_equal(y5_var_coords$x, y5_coords$x + 0.5)
  expect_equal(y5_int_coords$x, y5_coords$x + 0.5)
  # Secondary axis is 'y'. Side-by-side on 'y'.
  expect_equal(y5_var_coords$y, y5_coords$y - 0.25)
  expect_equal(y5_int_coords$y, y5_coords$y + 0.25)

  # --- MODERATOR NODE ---
  # Since a moderator has a parent (the anchor), it correctly gets the
  # flow-direction placement.

  # LR Layout
  config_lr <- configure_plot(FIXTURE_ANALYZED_MOD, recipe = list(rankdir = "LR"))
  layout_lr <- layout(config_lr) |> getElement("layout")

  mod_coords_lr <- layout_lr[id == "y1_y2"]
  mod_var_coords_lr <- layout_lr[id == "y1_y2_var"]
  mod_int_coords_lr <- layout_lr[id == "y1_y2_int"]

  # Primary axis is 'x'. Satellites offset on 'x'.
  expect_equal(mod_var_coords_lr$x, mod_coords_lr$x + 0.5)
  expect_equal(mod_int_coords_lr$x, mod_coords_lr$x + 0.5)
  # Secondary axis is 'y'. Side-by-side on 'y'.
  expect_equal(mod_var_coords_lr$y, mod_coords_lr$y - 0.25)
  expect_equal(mod_int_coords_lr$y, mod_coords_lr$y + 0.25)

  # TB Layout
  config_tb <- configure_plot(FIXTURE_ANALYZED_MOD, recipe = list(rankdir = "TB"))
  layout_tb <- layout(config_tb) |> getElement("layout")

  mod_coords_tb <- layout_tb[id == "y1_y2"]
  mod_var_coords_tb <- layout_tb[id == "y1_y2_var"]
  mod_int_coords_tb <- layout_tb[id == "y1_y2_int"]

  # Primary axis is 'y'. Satellites offset on 'y'.
  expect_equal(mod_var_coords_tb$y, mod_coords_tb$y - 0.5)
  expect_equal(mod_int_coords_tb$y, mod_coords_tb$y - 0.5)
  # Secondary axis is 'x'. Side-by-side on 'x'.
  expect_equal(mod_var_coords_tb$x, mod_coords_tb$x - 0.25)
  expect_equal(mod_int_coords_tb$x, mod_coords_tb$x + 0.25)
})

test_that("Satellite placement for ISOLATED nodes is correct (default)", {
  config <- configure_plot(FIXTURE_ANALYZED_ISO, recipe = list(rankdir = "LR"))
  layout_dt <- layout(config) |> getElement("layout")

  y_iso_coords <- layout_dt[id == "y_iso"]
  y_iso_var_coords <- layout_dt[id == "y_iso_var"]
  y_iso_int_coords <- layout_dt[id == "y_iso_int"]

  # Satellites on opposite sides of the secondary axis (y).
  expect_equal(y_iso_var_coords$y, y_iso_coords$y + 0.5)
  expect_equal(y_iso_int_coords$y, y_iso_coords$y - 0.5)
  # Side-by-side spread on the primary axis (x).
  expect_equal(y_iso_var_coords$x, y_iso_coords$x - 0.25)
  expect_equal(y_iso_int_coords$x, y_iso_coords$x + 0.25)
})
