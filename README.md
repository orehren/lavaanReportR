# lavaanReportR

`lavaanReportR` is an R package for creating publication-ready path diagrams
for structural equation models fitted with the `lavaan` package. It provides a
streamlined workflow for visualizing complex models, including multigroup and
multilevel models.

## Installation

You can install the development version of `lavaanReportR` from GitHub with:

```r
# install.packages("devtools")
devtools::install_github("orehren/lavaanReportR")
```

## Usage

The core workflow of `lavaanReportR` is a six-step pipeline:

1.  **`analyze()`**: Extracts the model structure from a `lavaan` parameter table.
2.  **`configure_plot()`**: Customizes the plot's appearance.
3.  **`layout()`**: Calculates the visual layout of the graph.
4.  **`prepare()`**: Prepares the final data for plotting.
5.  **`build()`**: Constructs the DOT code for the plot.
6.  **`render()`**: Renders the final plot.

Here's a basic example of how to use the package:

```r
library(lavaan)
library(lavaanReportR)

# 1. Fit a lavaan model
model <- '
  # measurement model
    ind60 =~ x1 + x2 + x3
    dem60 =~ y1 + y2 + y3 + y4
    dem65 =~ y5 + y6 + y7 + y8
  # structural model
    dem60 ~ ind60
    dem65 ~ ind60 + dem60
'
fit <- sem(model, data = PoliticalDemocracy)

# 2. Get the parameter table
param_table <- parameterEstimates(fit)

# 3. Run the lavaanReportR workflow
param_table |>
  analyze() |>
  configure_plot() |>
  layout() |>
  prepare() |>
  build() |>
  render()
```
