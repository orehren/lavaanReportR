#' @importFrom data.table as.data.table data.table .SD := fifelse setnames merge.data.table
#' @importFrom lavaan parameterEstimates standardizedSolution
#' @importFrom rlang %||%
NULL

# --- Internal Helper Functions ---

#' Create Standardized Parameter Estimates Table
#' @description Internal helper to get standardized estimates using `lavaan::standardizedSolution()`.
#' @param lavaan_object A fitted `lavaan` object.
#' @param ci Logical, whether to compute CIs.
#' @param ci_level Numeric, the confidence level.
#' @param estimates Character vector of base column names to be suffixed with ".std".
#' @return A `data.table` with standardized parameter estimates.
#' @noRd
.create_standardized_estimates_table <- function(lavaan_object, ci, ci_level, estimates) {
  std_est <- data.table::as.data.table(
    lavaan::standardizedSolution(lavaan_object, ci = ci, level = ci_level)
  )
  data.table::setnames(std_est, estimates, paste0(estimates, ".std"), skip_absent = TRUE)
  return(std_est)
}


#' Create Unstandardized Parameter Estimates Table
#' @description Internal helper to get unstandardized estimates using `lavaan::parameterEstimates()`.
#' @param lavaan_object A fitted `lavaan` object.
#' @param ci Logical, whether to compute CIs.
#' @param ci_level Numeric, the confidence level.
#' @param estimates Character vector of base column names to be suffixed with ".unstd".
#' @return A `data.table` with unstandardized parameter estimates.
#' @noRd
.create_unstandardized_estimates_table <- function(lavaan_object, ci, ci_level, estimates) {
  unstd_est <- data.table::as.data.table(
    lavaan::parameterEstimates(lavaan_object, ci = ci, standardized = FALSE, level = ci_level)
  )
  data.table::setnames(unstd_est, estimates, paste0(estimates, ".unstd"), skip_absent = TRUE)
  return(unstd_est)
}


#' Get (Un)standardized and Merged Estimates
#' @description Orchestrates the extraction of unstandardized and/or standardized
#' estimates and merges them if necessary.
#' @param lavaan_object A fitted `lavaan` object.
#' @param estimates_type One of "both", "standardized", or "unstandardized".
#' @param ci Logical, whether to compute CIs.
#' @param ci_level Numeric, the confidence level.
#' @return A `data.table` containing the requested estimates.
#' @noRd
.get_merged_estimates <- function(lavaan_object, estimates_type, ci, ci_level) {
  estimates_cols <- c("est", "se", "z", "pvalue", "ci.lower", "ci.upper")

  unstd_table <- .create_unstandardized_estimates_table(lavaan_object, ci, ci_level, estimates_cols)

  if (estimates_type == "unstandardized") {
    return(unstd_table)
  }

  std_table <- .create_standardized_estimates_table(lavaan_object, ci, ci_level, estimates_cols)

  if ("block" %in% names(unstd_table)) {
    std_table[, block := unstd_table$block]
  }

  if ("level" %in% names(unstd_table)) {
    std_table[, level := unstd_table$level]
  }

  if (estimates_type == "standardized") {
    return(std_table)
  } else {
    merge_keys <- intersect(names(unstd_table), names(std_table))
    return(data.table::merge.data.table(unstd_table, std_table, by = merge_keys))
  }
}


#' Modify and Clean a Lavaan Estimates Table
#' @description Performs post-processing on the extracted estimates table, including
#' factorizing columns, adding significance stars, and setting attributes.
#' @param data_object The `data.table` of estimates.
#' @param estimates_type Character string, used for setting an attribute.
#' @param ci Logical, used for setting an attribute.
#' @param ci_level Numeric, used for setting an attribute
#' @return The modified `data.table`.
#' @noRd
.modify_lavaan_estimates_table <- function(data_object, estimates_type, ci, ci_level) {

  # Step 1: Define and factorize grouping columns
  grouping_cols <- intersect(c("block", "group", "level", "exo", "label"), names(data_object))
  if (length(grouping_cols) > 0) {
    data_object[, (grouping_cols) := lapply(.SD, function(col) factor(col, levels = unique(col))), .SDcols = grouping_cols]
  }


  # Step 2a: Create a new character column `op_label` based on conditional logic.
  # data.table's `fifelse` is used for a fast, vectorized conditional assignment.
  data_object[, op_label := fcase(
    op == "=~", "Latent Variable",
    op == "~",  "Regression Path",
    # BEST-PRACTICE: Hier findet die entscheidende Unterscheidung statt.
    # Wenn op '~~' ist, prüfen wir, ob lhs und rhs identisch sind.
    op == "~~" & lhs == rhs, "Variance",
    op == "~~" & lhs != rhs, "Covariance",
    op == "~1", "Intercept",
    op == ":=", "Defined Parameter",
    default = as.character(op) # Fallback, falls neue Operatoren hinzukommen
  )]

  # Step 2b: Convert the new label column to a factor with a defined order.
  # Die alte `op`-Spalte wird dabei überschrieben.
  op_levels <- unique(data_object$op_label) # c("Latent Variable", "Regression Path", "Variance", "Covariance", "Intercept", "Defined Parameter")
  data_object[, op := factor(op_label, levels = op_levels)]
  data_object[, op_label := NULL] # Remove the temporary helper column

  # Step 3: Dynamically find the correct p-value column for significance stars
  p_col_for_sig <- if ("pvalue.unstd" %in% names(data_object)) {
    "pvalue.unstd"
  } else {
    "pvalue.std" # Fallback for standardized-only
  }
  data_object[, sig := fcase(
    is.na(get(p_col_for_sig)), " ",
    get(p_col_for_sig) < 0.001, "***",
    get(p_col_for_sig) < 0.01,  "**",
    get(p_col_for_sig) < 0.05,  "*",
    default = " "
  )]

  # Step 4: Clean up rhs/label for specific parameter types
  # REFAKTOR: Die Logik wurde angepasst, um den neuen Faktorlevel 'Covariances' zu berücksichtigen.
  # Die Bedingung `(op == "Variances" & rhs == lhs)` ist nun redundant, da wir dies bereits
  # in der Faktorerstellung getrennt haben.
  #data_object[op %in% c("Defined Parameters", "Variances"), rhs := " "]
  data_object[op == "Variances", rhs := ""]
  #data_object[op == "Defined Parameters", label := " "]

  # Step 5: Set attributes
  data.table::setattr(data_object, "estimates_included", estimates_type)
  if (isTRUE(ci)) {
    data.table::setattr(data_object, "ci_included", TRUE)
    data.table::setattr(data_object, "ci_level", ci_level)
  }

  return(data_object)
}


#' Filter and Select Columns for the Final Estimates Table
#' @description Filters rows based on the requested parameter types (`output`) and
#' selects the final set of columns based on `estimates_type`.
#' @param data_object The modified `data.table` of estimates.
#' @param output Character vector of parameter types to include (e.g., "Regression Paths").
#' @param estimates_type Character string indicating which estimate types are present.
#' @return A `data.table` with filtered rows and selected columns.
#' @noRd
.filter_lavaan_estimates_table <- function(data_object, output, estimates_type) {
  # Filter rows based on 'output' parameter
  if (!"all" %in% output) {
    data_object <- data_object[op %in% output]
  }

  # Build the list of columns to select in a readable way
  grouping_cols <- intersect(c("lhs", "op", "rhs", "block", "group", "level", "exo", "label"), names(data_object))
  cols_to_select <- grouping_cols

  if (estimates_type %in% c("unstandardized", "both")) {
    cols_to_select <- c(cols_to_select, names(data_object)[endsWith(names(data_object), ".unstd")])
  }
  if (estimates_type %in% c("standardized", "both")) {
    cols_to_select <- c(cols_to_select, names(data_object)[endsWith(names(data_object), ".std")])
  }
  cols_to_select <- c(cols_to_select, "sig")

  # Select and reorder columns
  data_object[, .SD, .SDcols = intersect(cols_to_select, names(data_object))]
}


#' Extract Parameter Estimates from a `lavaan` Model
#'
#' @description
#' Extracts a comprehensive set of parameter estimates (unstandardized and/or
#' standardized), standard errors, p-values, and confidence intervals from a
#' fitted `lavaan` model object. The function returns a clean `data.table`
#' ready for formatting.
#'
#' @param lavaan_object A fitted model object of class `lavaan`.
#' @param estimates_type A character string specifying which estimates to return.
#'   One of `"both"` (default), `"unstandardized"`, or `"standardized"`.
#' @param ci Logical. If `TRUE` (default), confidence intervals are included.
#' @param ci_level A single numeric value between 0 and 1 for the confidence
#'   level of intervals. Defaults to `0.95`.
#' @param output A character vector specifying which parameter types to include.
#'   Defaults to `"all"`. Allowed values are: `"Latent Variables"`,
#'   `"Regression Paths"`, `"Variances"`, `"Covariances"`, `"Intercepts"`,
#'   `"Defined Parameters"`.
#' @param save_output_to_file Logical. If `TRUE`, the resulting `data.table`
#'   will be saved to a file.
#' @param output_file_path The directory path for saving the data.
#' @param output_file_name The base name for the output file (without extension).
#' @param output_format The format for the saved data, one of "csv", "rds", "xlsx".
#'
#' @return A `data.table` containing the selected parameter estimates. The table
#'   has attributes `estimates_included` and `ci_included` for use by downstream
#'   formatting functions.
#'
#' @export
#'
#' @examples
#' if (requireNamespace("lavaan", quietly = TRUE)) {
#'   HS.model <- ' visual  =~ x1 + x2 + x3
#'                 textual =~ x4 + x5 + x6
#'                 speed   =~ x7 + x8 + x9 '
#'   fit <- lavaan::cfa(HS.model, data = lavaan::HolzingerSwineford1939)
#'
#'   # Extract only standardized regression paths without CIs
#'   estimates <- extract_model_estimates_data(
#'     fit,
#'     estimates_type = "standardized",
#'     ci = FALSE,
#'     output = "Regression Paths"
#'   )
#'   print(estimates)
#' }
extract_model_estimates_data <- function(lavaan_object,
                                         estimates_type = "both",
                                         ci = TRUE,
                                         ci_level = 0.95,
                                         output = "all",
                                         save_output_to_file = FALSE,
                                         output_file_path = NULL,
                                         output_file_name = "model_estimates_data",
                                         output_format = "rds") {
  # --- 1. Input Validation ---
  # args_list <- .get_caller_args()
  # args_list$data_object_type <- "lavaan"
  # rlang::exec(.validate_function_args, !!!args_list)

  # --- 2. Data Extraction and Merging ---
  estimates_data <- .get_merged_estimates(lavaan_object, estimates_type, ci, ci_level)

  # --- 3. Data Modification and Cleaning ---
  estimates_data <- .modify_lavaan_estimates_table(estimates_data, estimates_type, ci, ci_level)

  # --- 4. Filtering and Column Selection ---
  final_data <- .filter_lavaan_estimates_table(estimates_data, output, estimates_type)

  # --- 5. Mark data.table as mlmReportR created object for save_analysis function ---
  # attr(final_data, "created_by") <- "mlmReportR"
  # final_data <- tag_as_mlmReport(final_data)
  class(final_data) <- c("lavaan_parameter_table", class(final_data))

  # --- 6. Save to File if Requested ---
  if (save_output_to_file) {
    .save_data_to_file(final_data, output_file_path, output_file_name, output_format)
  }

  return(final_data)
}


#' Create a Formatted Table of Parameter Estimates
#'
#' @description
#' Takes a `data.table` from `extract_model_estimates_data` and creates a
#' publication-ready `gt` table using a flexible configuration system. This
#' function acts as a user-friendly wrapper around a generic table-making engine.
#'
#' @param data_object A `data.table` as produced by `extract_model_estimates_data()`.
#' @param table_format A character string, either `"long"` (default) or `"wide"`.
#'   `"long"` format uses row groups for sections, while `"wide"` format
#'   reshapes the data to use column spanners.
#' @param groupname_col Tidyselect expression for the column to be used for `gt`'s
#'   row groups. Defaults to `Section`. Only used if `table_format` is `"long"`.
#' @param rowname_col Tidyselect expression for the column to be used for `gt`'s
#'   row stubs. Defaults to `Measure` in long format and `Type` in wide format.
#' @param col_names An optional named list or vector for relabeling columns,
#'   passed to `gt::cols_label()`. Example: `list(default = "Value")`.
#' @param hide_cols Optional. A character vector or tidyselect expression of
#'   columns to hide from the final table.
#' @param align_decimal Logical. If `TRUE`, numeric columns will be aligned at
#'   the decimal point. Defaults to `FALSE`.
#' @param decimals The number of decimal places to display for numeric values.
#' @param substitute_pvalues Logical. If `TRUE`, numeric p-values are formatted
#'   into character strings (e.g., "< .001"). Defaults to `TRUE`.
#' @param table_caption An optional character string for the table's main title.
#' @param source_note An optional character string for the table's source note.
#' @param footnote_specs An optional list of footnote specifications. Use `"default"`
#'   to apply a set of predefined, context-aware footnotes. To add custom
#'   footnotes, provide a list of specifications created with
#'   `gt_create_footnote_spec()`. Set to `NULL` to disable all footnotes.
#' @param save_output_to_file Logical. If `TRUE`, the `gt` table will be saved.
#' @param output_file_path The directory path for saving the table.
#' @param output_file_name The base name for the file to save.
#' @param output_format The format for the saved table (e.g., "html", "docx").
#'
#' @return A `gt_tbl` object, ready for printing or saving.
#'
#' @export
#'
#' @examples
#' if (requireNamespace("lavaan", quietly = TRUE)) {
#'   HS.model <- ' visual  =~ x1 + x2 + x3
#'                 textual =~ x4 + x5 + x6
#'                 speed   =~ x7 + x8 + x9 '
#'   fit <- lavaan::cfa(HS.model, data = lavaan::HolzingerSwineford1939)
#'   estimates_data <- extract_model_estimates_data(fit)
#'
#'   # Create a table with custom column names and a caption
#'   \dontrun{
#'   estimates_data <- create_model_estimates_table(
#'     estimates_data,
#'     table_caption = "CFA Parameter Estimates",
#'     col_names = list(
#'       rhs = " ",
#'       est.unstd = "B",
#'       se.unstd = "SE",
#'       pvalue.unstd = "p",
#'       est.std = "Beta"
#'     ),
#'     hide_cols = c(z.unstd, z.std, pvalue.std)
#'   )
#'   # print(estimates_data)
#'   }
#' }
create_model_estimates_table <- function(data_object, ...) {

  user_args <- list(...)

  # Determine dynamic context from the data object and user arguments
  user_args$table_format <- attr(data_object, "estimates_included", exact = TRUE) %||% "unstandardized"
  ci_included <- attr(data_object, "ci_included", exact = TRUE) %||% FALSE
  ci_level <- attr(data_object, "ci_level", exact = TRUE) %||% user_args$ci_level %||% 0.95

  # Pass this dynamic context to the builder via user_args
  user_args$spanner_fn_args <- list(ci_included = ci_included, ci_level = ci_level)

  # The builder needs to know which mode to select from the template
  # user_args$table_format <- estimates_type

  .gt_build_publication_table(
    data_object = data_object,
    user_args = user_args,
    template = config_template_model_estimates,
    data_object_type = "gt"
  )
}
