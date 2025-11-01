library(data.table)

context("Analysis Phase")

test_that("Defined path analysis correctly identifies start and end nodes", {
  # 1. Define a simple mediation model using actual variable names from the dataset
  model <- "
    # direct effect
    x3 ~ c*x1
    # mediator
    x2 ~ a*x1
    x3 ~ b*x2
    # indirect effect (a*b)
    ab := a*b
    # total effect
    total := c + (a*b)
  "

  # 2. Fit the model with lavaan
  fit <- lavaan::sem(model, data = lavaan::HolzingerSwineford1939)

  # 3. Get the parameter table using the package's internal function
  param_table <- lavaanReportR::extract_model_estimates_data(fit)

  # 4. Run the defined path analysis
  # Note: The .analyze_defined_paths_structure function expects a data.table,
  # and the output of extract_model_estimates_data is a lavaan_parameter_table object,
  # which is a data.table, so this should work directly.
  defined_paths_result <- lavaanReportR:::.analyze_defined_paths_structure(param_table)

  # 5. Assertions
  # Check the indirect path 'ab'
  indirect_path <- defined_paths_result[label == "ab"]
  expect_equal(indirect_path$from, "x1")
  expect_equal(indirect_path$to, "x3")

  # Check the total path 'total'
  total_path <- defined_paths_result[label == "total"]
  expect_equal(total_path$from, "x1")
  expect_equal(total_path$to, "x3")
})
