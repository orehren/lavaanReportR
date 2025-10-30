# Load necessary packages
library(data.table)
devtools::load_all()

# Define and fit the model
fit_moderation_simple <- lavaan::sem(
    "y4 ~ y1 + y1:y4",
    data = lavaan::PoliticalDemocracy,
    parallel = "multicore",
    ncpus = 1
)

# Run the pipeline and capture the build object
build_object <- fit_moderation_simple |>
  extract_model_estimates_data() |>
  analyze() |>
  configure_plot() |>
  prepare() |>
  build()

# Print the final dot code for inspection
cat(build_object$dot_code)
