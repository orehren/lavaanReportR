library(data.table)

context("Full Pipeline Integration")

test_that("The full analyze -> configure -> layout -> prepare pipeline runs without error", {
  # 1. Define and fit a simple model
  model <- "x3 ~ x1 + x2"
  fit <- lavaan::sem(model, data = lavaan::HolzingerSwineford1939)

  # 2. Get the parameter table using the package's function
  param_table <- lavaanReportR::extract_model_estimates_data(fit)

  # 3. Run the full pipeline
  # We expect this to run without any S3 dispatch errors
  final_graph_object <- param_table |>
    lavaanReportR::analyze() |>
    lavaanReportR::configure_plot() |>
    lavaanReportR::layout() |>
    lavaanReportR::prepare()

  # 4. Assertions
  # Check that the final object has the correct class
  expect_true("lavaan_graph" %in% class(final_graph_object))

  # Check that the final object contains the expected components
  expect_true("nodes" %in% names(final_graph_object))
  expect_true("edges" %in% names(final_graph_object))
  expect_true("recipe" %in% names(final_graph_object))

  # Check that the components are not empty
  expect_true(nrow(final_graph_object$nodes) > 0)
  expect_true(nrow(final_graph_object$edges) > 0)
})
