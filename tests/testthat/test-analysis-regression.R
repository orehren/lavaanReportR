# Load necessary libraries
library(devtools)
library(testthat)
library(data.table)
library(purrr) # compact() is from purrr

# Load internal data
load("R/sysdata.rda")

# Source all R files from the correct path
lapply(list.files("R", pattern = "\\.R$", full.names = TRUE), source)

# Define the parameter tables provided by the user, using simple character vectors for 'op'
pt_moderation_simple <- data.table(
  lhs = c("y1", "y1", "y1:y2", "y1:y2", "y2", "y2", "y3", "y3", "y3", "y3", "y3"),
  op = c("~1", "~~", "~1", "~~", "~1", "~~", "~", "~", "~", "~1", "~~"),
  rhs = c("", "y1", "", "y1:y2", "", "y2", "y1", "y1:y2", "y2", "", "y3"),
  est.unstd = c(5.46, 6.78, 29.42, 937.89, 4.25, 15.37, 0.83, -0.01, 0.13, 1.79, 5.69)
)
# Set the correct class for S3 dispatch
setattr(pt_moderation_simple, "class", c("lavaan_parameter_table", "data.table", "data.frame"))


pt_path_simple <- data.table(
  lhs = c("y1", "y1", "y2", "y2", "y2", "y2", "y3", "y3", "y3", "y4", "y4", "y4", "y4"),
  op = c("~1", "~~", "~", "~1", "~~", "~~", "~1", "~~", "~~", "~", "~", "~1", "~~"),
  rhs = c("", "y1", "y1", "", "y1", "y2", "", "y1", "y3", "y2", "y3", "", "y4"),
  est.unstd = c(5.46, 6.78, 0.99, -1.20, -0.61, 9.82, 6.56, 5.76, 10.62, 0.47, 0.36, 0.04, 4.22)
)
# Set the correct class for S3 dispatch
setattr(pt_path_simple, "class", c("lavaan_parameter_table", "data.table", "data.frame"))


test_that("analyze() handles moderation models correctly", {
  expect_no_error(analyze(pt_moderation_simple))
})

test_that("analyze() handles simple path models without moderation correctly", {
  expect_no_error(analyze(pt_path_simple))
})
