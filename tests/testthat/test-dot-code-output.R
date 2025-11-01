# Load necessary libraries
library(devtools)
library(testthat)
library(data.table)
library(purrr)

# Load internal data
load("../../R/sysdata.rda")

# Source all R files
lapply(list.files("../../R", pattern = "\\.R$", full.names = TRUE), source)

test_that("Dot code for nodes does not contain redundant x and y attributes", {
  # 1. Define a simple model and create the parameter table using the correct function
  simple_model_ptable <- lavaan::sem(
    'y2 ~ y1',
    data = lavaan::PoliticalDemocracy
  ) |> extract_model_estimates_data()

  # 2. Run the full pipeline to get the final dot code
  dot_code <- simple_model_ptable |>
    analyze() |>
    configure_plot() |>
    layout() |>
    prepare() |>
    build()

  # 3. Extract all attribute definition lines from the dot code string
  all_attr_lines <- regmatches(
    dot_code$dot_code,
    gregexpr("([^\\s]+)\\s*\\[(.*?)\\];", dot_code$dot_code, perl = TRUE)
  )[[1]]

  # Filter to get ONLY node lines by looking for the 'shape' attribute
  node_attr_lines <- grep("shape=", all_attr_lines, value = TRUE)

  # 4. Assert that the `pos` attribute is present in all node lines
  expect_true(all(grepl("pos=", node_attr_lines)))

  # 5. Assert that the redundant `x=` and `y=` attributes are NOT present
  expect_false(any(grepl("\\sx=", node_attr_lines)))
  expect_false(any(grepl("\\sy=", node_attr_lines)))
})
