# lavaanReportR

`lavaanReportR` is an R package for creating publication-ready path diagrams
for structural equation models fitted with the `lavaan` package. It provides a
streamlined, five-step workflow for visualizing complex models, including
multigroup, multilevel, and mediation models.

## Features

- **Five-Step Pipeline**: A clear and transparent workflow (`analyze()`,
  `configure_plot()`, `prepare()`, `build()`, `render()`) that makes it easy to
  create and customize plots.
- **Advanced Customization**: A powerful `configure_plot()` function and a
  'Recipe' system for fine-grained control over the plot's appearance.
- **Complex Model Support**: Automatically handles multigroup, multilevel, and
  mediation models.
- **Publication-Ready**: Produces high-quality diagrams suitable for inclusion
  in academic papers and presentations.

## Installation

You can install the development version of `lavaanReportR` from GitHub with:

```r
# install.packages("devtools")
devtools::install_github("orehren/lavaanReportR")
```

## Getting Started

The core workflow of `lavaanReportR` is a five-step pipeline. Here's a quick
example of how to use it:

```r
library(lavaan)
library(lavaanReportR)

# 1. Fit a lavaan model
model <- '
  # Measurement Model
  ind60 =~ x1 + x2 + x3
  dem60 =~ y1 + y2 + y3 + y4
  dem65 =~ y5 + y6 + y7 + y8
  # Structural Model
  dem60 ~ ind60
  dem65 ~ ind60 + dem60
'
fit <- sem(model, data = PoliticalDemocracy)

# 2. Get the parameter table
param_table <- parameterEstimates(fit)

# 3. Run the lavaanReportR workflow
param_table |>
  analyze() |>
  configure_plot(
    plot_flow_direction = "LR",
    node_labels = list(
      ind60 = "Industrialization 1960",
      dem60 = "Democracy 1960",
      dem65 = "Democracy 1965"
    )
  ) |>
  prepare() |>
  build() |>
  render()
```

## Gallery

Here are a few examples of the types of plots you can create with `lavaanReportR`.

*A simple CFA model*
<img src="https://lavaan.ugent.be/images/cfa_3_factors.svg" width="500">

*A mediation model with custom labels*
<img src="https://lavaan.ugent.be/images/mediation.svg" width="500">

*A multigroup model*
<img src="https://lavaan.ugent.be/images/multigroup_sem.svg" width="500">

## Learning More

To learn more about how to use `lavaanReportR`, please see the following
vignettes:

- **Getting Started**: `vignette("introduction-to-lavaanReportR", package = "lavaanReportR")`
- **Advanced Customization**: `vignette("advanced-customization", package = "lavaanReportR")`
