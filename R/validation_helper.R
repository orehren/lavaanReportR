# ==============================================================================
# SECTION: VALIDATION HELPERS
# ==============================================================================


# ------------------------------------------------------------------------------
# Imports
# ------------------------------------------------------------------------------

#' @importFrom data.table as.data.table data.table .SD := fifelse setnames merge.data.table
#' @importFrom lavaan parameterEstimates standardizedSolution
#' @importFrom rlang %||% is_expression is_call is_symbol quo is_quosure
NULL

# ------------------------------------------------------------------------------
# Master Validation Function (Engine)
# ------------------------------------------------------------------------------

#' Master Validation Function (Engine)
#'
#' @description
#' Acts as a central dispatcher for input validation. It iterates through the
#' provided arguments, looks up the corresponding rule in the `validation_rules`
#' "rule book", and executes the validation dynamically.
#' @return Invisibly returns `TRUE` if all checks pass.
#' @noRd
.validate_function_args <- function(...) {
  args_list <- list(...)

  context <- args_list$data_object_type
  if (is.null(context)) stop("Validation context 'data_object_type' must be provided.", call. = FALSE)

  valid_output_formats <- switch(context,
    "gt" = c("html", "htm", "pdf", "docx", "tex", "rnw", "rtf", "png", "rds"),
    c("csv", "rds", "xlsx", "sav", "dta", "sas")
  )

  data_object_class <- switch(context,
    "lavaan" = "lavaan",
    "reliability" = "data.frame",
    "reliability_extract" = "list",
    "sem_plot" = "data.frame",
    "data.table"
  )

  for (arg_name in names(args_list)) {
    rule <- validation_rules[[arg_name]]
    if (is.null(rule)) next

    arg_value <- args_list[[arg_name]]

    validator_args <- list(
      arg_value,
      arg_name_for_msg = arg_name
    )
    validator_args <- c(validator_args, rule$args)

    if (isTRUE(rule$is_contextual)) {
      contextual_args <- switch(arg_name,
        data_object = ,
        table_object = list(expected_class = data_object_class),
        x = list(expected_class = "psych"),
        groupname_col = ,
        rowname_col = ,
        # hide_cols = list(allowed_strings = names(args_list$data_object)),
        output_format = list(allowed_strings = valid_output_formats),
        # footnote_specs = list(data_colnames = names(args_list$data_object)),
        # node_labels = list(allowed_strings = unique(c(args_list$data_object$lhs, args_list$data_object$rhs))),
        # effect_labels = list(allowed_strings = unique(args_list$data_object[, label])), # label != "", label
        hide_cols = list(allowed_strings = .get_allowed_strings_from_data(args_list$data_object, source = "names")),
        footnote_specs = list(data_colnames = .get_allowed_strings_from_data(args_list$data_object, source = "names")),
        node_labels = list(allowed_strings = .get_allowed_strings_from_data(args_list$data_object, source = "values", cols = c("lhs", "rhs"), filter_empty = TRUE)),
        effect_labels = list(allowed_strings = .get_allowed_strings_from_data(args_list$data_object, source = "values", cols = c("lhs", "rhs"), filter_empty = TRUE)),
        "..." = list(data = args_list$data),
        list()
      )
      validator_args <- c(validator_args, contextual_args)
    }

    if (isTRUE(rule$is_quo)) {
      if (!rlang::is_quosure(validator_args[[1]])) {
        validator_args[[1]] <- rlang::quo(!!validator_args[[1]])
      }
    }

    rlang::exec(rule$validator_fn, !!!validator_args)
  }

  if (isTRUE(args_list$save_output_to_file) && is.null(args_list$output_file_name)) {
    stop("Argument 'output_file_name' must be provided if 'save_output_to_file' is TRUE.", call. = FALSE)
  }

  invisible(TRUE)
}

# ------------------------------------------------------------------------------
# Engine Helpers
# ------------------------------------------------------------------------------

#' @title Get Allowed Strings from a Data Object
#' @description A general-purpose helper to extract a character vector of allowed
#'   strings from a data object, based on specified rules.
#' @param data_object The data.table to extract from.
#' @param source A character string: "names" to get column names, or "values"
#'   to get unique values from specified columns.
#' @param cols A character vector of column names to use when `source = "values"`.
#' @param filter_empty A logical flag. If `TRUE`, empty strings are removed.
#' @return A character vector of allowed strings.
#' @keywords internal
#' @noRd
.get_allowed_strings_from_data <- function(data_object, source, cols = NULL, filter_empty = FALSE) {
  allowed_strings <- switch(source,
    "names" = names(data_object),
    "values" = {
      valid_cols <- intersect(cols, names(data_object))

      if (length(valid_cols) == 0) {
        return(character(0))
      }

      unique(unlist(data_object[, ..valid_cols]))
    },
    stop("Internal validation error: Invalid 'source' specified in .get_allowed_strings_from_data.", call. = FALSE)
  )


  if (filter_empty) {
    allowed_strings <- allowed_strings[!is.na(allowed_strings)]
    allowed_strings <- allowed_strings[nzchar(allowed_strings)]
  }

  return(allowed_strings)
}


#' Get All Arguments from the Calling Function
#'
#' @description
#' A metaprogramming helper that inspects the execution frame of its caller.
#' It retrieves a complete, named list of all arguments—both formal and those
#' passed via the ellipsis (`...`)—and their evaluated values.
#' @return A named list of all arguments and their values from the caller's environment.
#' @noRd
.get_caller_args <- function() {
  caller_function <- try(sys.function(-1), silent = TRUE)
  if (inherits(caller_function, "try-error") || !rlang::is_function(caller_function)) {
    stop(".get_caller_args() could not identify the calling function.", call. = FALSE)
  }
  caller_env <- parent.frame()

  caller_formals <- formals(caller_function)
  caller_arg_names <- names(caller_formals)

  named_formal_args <- setdiff(caller_arg_names, "...")
  named_args_list <- mget(named_formal_args, envir = caller_env, ifnotfound = list(NULL))

  ellipsis_args <- if ("..." %in% caller_arg_names) {
    eval(rlang::expr(list(...)), envir = caller_env)
  } else {
    list()
  }

  c(named_args_list, ellipsis_args)
}


# ------------------------------------------------------------------------------
# Validation Functions
# ------------------------------------------------------------------------------

# ------- Class Type Validators ------------------------------------------------

#' Assert that an Argument is of a Specific Class
#'
#' @description Checks if an object inherits from an expected class.
#' @param allow_null Logical. If `TRUE`, a `NULL` value for `obj_to_check`
#'   will pass the validation.
#' @return Invisibly returns `NULL` if validation passes, otherwise stops with an error.
#' @noRd
.assert_argument_is_class <- function(obj_to_check,
                                      expected_class,
                                      arg_name_for_msg,
                                      allow_null = TRUE) {
  if (allow_null && is.null(obj_to_check)) {
    return(invisible(NULL))
  }

  if (!inherits(obj_to_check, expected_class)) {
    stop(sprintf(
      "Argument '%s' must be an object of class '%s', but you gave '%s', which is of class '%s'.",
      arg_name_for_msg,
      expected_class,
      deparse1(obj_to_check, collapse = " "),
      paste(class(obj_to_check), collapse = "/")
    ), call. = FALSE)
  }
  invisible(NULL)
}


#' Assert that All Elements in a List are of a Specific Class
#'
#' @description Checks if every element in a list inherits from an expected class.
#' @return Invisibly returns `NULL` if validation passes, otherwise stops with an error.
#' @noRd
.assert_list_elements_are_class <- function(list_to_check,
                                            expected_class,
                                            arg_name_for_msg,
                                            allow_null = TRUE) {
  if (is.null(list_to_check) && isTRUE(allow_null)) {
    return(invisible(NULL))
  }

  if (!is.list(list_to_check)) {
    stop(sprintf(
      "Argument '%s' was expected to be a list for validation, but it is a '%s'.",
      arg_name_for_msg, class(list_to_check)[1]
    ), call. = FALSE)
  }

  for (i in seq_along(list_to_check)) {
    element <- list_to_check[[i]]

    if (!inherits(element, expected_class)) {
      stop(sprintf(
        "Each element of argument '%s' must be of class '%s'. \nElement %d is of class '%s'.",
        arg_name_for_msg,
        expected_class,
        i,
        paste(class(element), collapse = "/")
      ), call. = FALSE)
    }
  }

  invisible(NULL)
}


# ------- Character Type Validators --------------------------------------------

#' Assert that an Argument is a Character Vector
#'
#' @description Checks if an argument is a character vector.
#' @return Invisibly returns `NULL` if validation passes, otherwise stops with an error.
#' @noRd
.assert_argument_is_character_vector <- function(arg_value,
                                                 arg_name_for_msg,
                                                 allow_null = TRUE) {
  if (is.null(arg_value) && allow_null) {
    return(invisible(NULL))
  }

  if (!is.character(arg_value)) {
    stop(sprintf(
      "Argument '%s' must be a character vector, but it is of class '%s'.",
      arg_name_for_msg,
      class(arg_value)[1]
    ), call. = FALSE)
  }
  invisible(NULL)
}


# ------- Logical Type Validators ----------------------------------------------


#' Assert that an Argument is a Single Logical Flag
#'
#' @description Checks if an argument is a single `TRUE` or `FALSE` value (and not `NA`).
#' @return Invisibly returns `NULL` if validation passes, otherwise stops with an error.
#' @noRd
.assert_argument_is_flag <- function(arg_value,
                                     arg_name_for_msg,
                                     allow_null = TRUE) {
  if (is.null(arg_value) && allow_null) {
    return(invisible(NULL))
  }

  if (!(isTRUE(arg_value) || isFALSE(arg_value))) {
    stop(sprintf(
      "Argument '%s' must be a single logical value (TRUE or FALSE). \nReceived: %s.",
      arg_name_for_msg,
      if (is.logical(arg_value) && length(arg_value) == 1) as.character(arg_value) else paste0("type ", typeof(arg_value), " (value: ", deparse1(arg_value, nlines = 1, width.cutoff = 20), ")")
    ), call. = FALSE)
  }
  invisible(NULL)
}


# ------- Numeric Type Validators ----------------------------------------------

#' @title Assert that an Argument is a Single Numeric Value
#' @description A flexible checker for single numeric values with options to
#'   control the validity of NULL, NA, negative values, and a specific range.
#' @param arg_value Value to check.
#' @param arg_name_for_msg Name of the argument for the error message.
#' @param allow_null If `TRUE`, `NULL` is a valid value.
#' @param allow_na If `TRUE`, a single `NA` value is valid.
#' @param allow_negative_value If `TRUE`, negative numbers are allowed.
#' @param allowed_range A numeric vector of length 2, `c(min, max)`, defining an
#'   inclusive range. If `NULL` (default), no range check is performed.
#' @return Invisibly returns `NULL` if validation passes.
#' @keywords internal
#' @noRd
.assert_argument_is_single_numeric <- function(arg_value,
                                               arg_name_for_msg,
                                               allow_null = TRUE,
                                               allow_na = TRUE,
                                               allow_negative_value = TRUE,
                                               allowed_range = NULL) {
  # Case 1: Handle NULL
  if (allow_null && is.null(arg_value)) {
    return(invisible(NULL))
  }

  # Case 2: Handle NA
  if (allow_na && length(arg_value) == 1 && is.na(arg_value)) {
    return(invisible(NULL))
  }

  # Case 3: Check for basic numeric properties
  if (!is.numeric(arg_value) || length(arg_value) != 1 || is.na(arg_value)) {
    stop(sprintf(
      "Argument '%s' must be a single, non-NA numeric value. Received: %s.",
      arg_name_for_msg, deparse1(arg_value)
    ), call. = FALSE)
  }

  # Case 4: Check for negative values
  if (!allow_negative_value && arg_value < 0) {
    stop(sprintf(
      "Argument '%s' must be a non-negative numeric value (>= 0). Received: %s.",
      arg_name_for_msg, arg_value
    ), call. = FALSE)
  }

  if (is.null(allowed_range)) {
    return(invisible(NULL))
  }

  # Case 5: Check for range ---
  # Internal validation of the rule itself.
  if (!is.numeric(allowed_range) || length(allowed_range) != 2) {
    stop("Internal validation error: 'allowed_range' must be a numeric vector of length 2.", call. = FALSE)
  }

  min_val <- allowed_range[1]
  max_val <- allowed_range[2]

  if (arg_value < min_val || arg_value > max_val) {
    stop(sprintf(
      "Argument '%s' must be within the range [%s, %s]. Received: %s.",
      arg_name_for_msg, min_val, max_val, arg_value
    ), call. = FALSE)
  }

  invisible(NULL)
}


# ------- Integer Type Validators ----------------------------------------------


#' @title Assert that an Argument is a Single Integer
#' @description A flexible checker for single integer values with options to
#'   control the validity of NULL, NA, negative values, and a specific range.
#' @param arg_value Value to check.
#' @param arg_name_for_msg Name of the argument for the error message.
#' @param allow_null If `TRUE`, `NULL` is a valid value.
#' @param allow_na If `TRUE`, a single `NA` value is valid.
#' @param allow_negative_value If `TRUE`, negative integers are allowed.
#' @param allowed_range An integer vector of length 2, `c(min, max)`, defining an
#'   inclusive range. If `NULL` (default), no range check is performed.
#' @return Invisibly returns `NULL` if validation passes.
#' @keywords internal
#' @noRd
.assert_argument_is_single_integer <- function(arg_value,
                                               arg_name_for_msg,
                                               allow_null = TRUE,
                                               allow_na = TRUE,
                                               allow_negative_value = TRUE,
                                               allowed_range = NULL) {
  # Case 1: Handle NULL
  if (allow_null && is.null(arg_value)) {
    return(invisible(NULL))
  }

  # Case 2: Handle NA
  if (allow_na && length(arg_value) == 1 && is.na(arg_value)) {
    return(invisible(NULL))
  }

  # Case 3: Check for basic numeric properties
  if (!is.numeric(arg_value) || length(arg_value) != 1 || is.na(arg_value)) {
    stop(sprintf(
      "Argument '%s' must be a single, non-NA integer value. Received: %s.",
      arg_name_for_msg, deparse1(arg_value)
    ), call. = FALSE)
  }

  # --- NEU: Case 4: Check if it's a whole number ---
  is_whole_number <- abs(arg_value - round(arg_value)) < .Machine$double.eps^0.5
  if (!is_whole_number) {
    stop(sprintf(
      "Argument '%s' must be a whole number (integer). Received: %s.",
      arg_name_for_msg, arg_value
    ), call. = FALSE)
  }

  # Case 5: Check for negative values
  if (!allow_negative_value && arg_value < 0) {
    stop(sprintf(
      "Argument '%s' must be a non-negative integer (>= 0). Received: %s.",
      arg_name_for_msg, arg_value
    ), call. = FALSE)
  }

  # Case 6: Check for range
  if (is.null(allowed_range)) {
    return(invisible(NULL))
  }

  if (!is.numeric(allowed_range) || length(allowed_range) != 2) {
    stop("Internal validation error: 'allowed_range' must be an integer vector of length 2.", call. = FALSE)
  }

  min_val <- allowed_range[1]
  max_val <- allowed_range[2]

  if (arg_value < min_val || arg_value > max_val) {
    stop(sprintf(
      "Argument '%s' must be an integer within the range [%s, %s]. Received: %s.",
      arg_name_for_msg, min_val, max_val, arg_value
    ), call. = FALSE)
  }

  invisible(NULL)
}


# ------- Specific Type Validators ---------------------------------------------


#' Assert that a Quoted Argument for String Selection is Plausible
#'
#' @description
#' Performs basic checks on a quosure intended for tidyselect-style string
#' selection.
#' @return Invisibly returns `NULL` if validation passes, otherwise stops with an error.
#' @keywords internal
.assert_argument_is_plausible_string_selector <- function(quo_string_selection,
                                                          allowed_strings,
                                                          arg_name_for_msg,
                                                          allow_null = TRUE) {
  if (rlang::quo_is_null(quo_string_selection) || rlang::quo_is_missing(quo_string_selection)) {
    if (allow_null) {
      return(invisible(NULL))
    } else {
      stop(sprintf("Argument '%s' cannot be NULL or missing.", arg_name_for_msg), call. = FALSE)
    }
  }

  expr_string_selection <- rlang::quo_get_expr(quo_string_selection)

  if (is.character(expr_string_selection)) {
    if (!all(expr_string_selection %in% allowed_strings)) {
      missing_strings <- setdiff(expr_string_selection, allowed_strings)
      stop(sprintf(
        "Argument '%s' contains invalid value(s): %s. \nAllowed values: %s.",
        arg_name_for_msg,
        paste(dQuote(missing_strings), collapse = ", "),
        paste(dQuote(allowed_strings), collapse = ", ")
      ), call. = FALSE)
    }
  } else if (rlang::is_call(expr_string_selection) && rlang::call_name(expr_string_selection) == "c") {
    call_args <- rlang::call_args(expr_string_selection)
    for (arg_item in call_args) {
      if (is.character(arg_item)) {
        if (!all(arg_item %in% allowed_strings)) {
          missing_strings <- setdiff(arg_item, allowed_strings)
          stop(sprintf(
            "In argument '%s', a character vector within c() contains invalid value(s): %s. \nAllowed values: %s.",
            arg_name_for_msg,
            paste(dQuote(missing_strings), collapse = ", "),
            paste(dQuote(allowed_strings), collapse = ", ")
          ), call. = FALSE)
        }
      } else if (rlang::is_symbol(arg_item)) {
        if (!as.character(arg_item) %in% allowed_strings) {
          stop(sprintf(
            "In argument '%s', the name '%s' provided within c() is not a valid value. \nValid values: %s.",
            arg_name_for_msg,
            as.character(arg_item),
            paste(dQuote(allowed_strings), collapse = ", ")
          ), call. = FALSE)
        }
      }
    }
  } else if (rlang::is_symbol(expr_string_selection)) {
    if (!as.character(expr_string_selection) %in% allowed_strings) {
      stop(sprintf(
        "In argument '%s', the value '%s' is not valid. \nValid values: %s.",
        arg_name_for_msg,
        as.character(expr_string_selection),
        paste(dQuote(allowed_strings), collapse = ", ")
      ), call. = FALSE)
    }
  } else if (rlang::is_call(expr_string_selection) || is.numeric(expr_string_selection)) {
    invisible(NULL)
  } else {
    stop(sprintf(
      "Argument '%s' has an unexpected type or structure: %s. \nExpected NULL, character vector, symbol, or a call (e.g., starts_with(), c()).",
      arg_name_for_msg,
      rlang::as_label(quo_string_selection)
    ), call. = FALSE)
  }
  invisible(NULL)
}


#' Assert that a Quoted Argument for Column/Choice Selection is Plausible or a Specific Keyword
#'
#' @description This function wraps [`.assert_argument_is_plausible_string_selector()`].
#' @return Invisibly returns `NULL` if validation passes.
#' @noRd
.assert_argument_is_plausible_string_selector_or_all <- function(quo_string_selection,
                                                                 allowed_strings,
                                                                 arg_name_for_msg,
                                                                 allow_null = TRUE,
                                                                 all_keyword = "all") {
  expr_string_selection <- rlang::quo_get_expr(quo_string_selection)

  if (is.character(expr_string_selection) &&
    length(expr_string_selection) == 1 &&
    expr_string_selection == all_keyword) {
    invisible(NULL)
  } else {
    .assert_argument_is_plausible_string_selector(
      quo_string_selection,
      allowed_strings,
      arg_name_for_msg,
      allow_null
    )
  }
}

#

#' Assert that a Quoted Argument for Column/Choice Selection is a single Plausible or a Specific Keyword
#'
#' @description This function wraps [`.assert_argument_is_plausible_string_selector()`].
#' @return Invisibly returns `NULL` if validation passes.
#' @noRd
.assert_argument_is_single_plausible_string_selector <- function(quo_string_selection,
                                                                 allowed_strings,
                                                                 arg_name_for_msg,
                                                                 allow_null = TRUE) {
  if (rlang::quo_is_null(quo_string_selection) || rlang::quo_is_missing(quo_string_selection)) {
    if (allow_null) {
      return(invisible(NULL))
    } else {
      stop(sprintf("Argument '%s' cannot be NULL or missing.", arg_name_for_msg), call. = FALSE)
    }
  }

  expr_string_selection <- rlang::quo_get_expr(quo_string_selection)

  if (length(expr_string_selection) > 1) {
    stop(sprintf(
      "Argument '%s' only allows for a single value. '%s' is not a single value. \nValid values: %s.",
      arg_name_for_msg,
      as.character(expr_string_selection),
      paste(dQuote(allowed_strings), collapse = ", ")
    ), call. = FALSE)
  } else {
    .assert_argument_is_plausible_string_selector(
      quo_string_selection,
      allowed_strings,
      arg_name_for_msg,
      allow_null
    )
  }
}


#' Validate a List of Footnote Specifications
#'
#' @description Checks if `footnote_specs_list` is a list where each element is a sub-list
#' containing valid 'text', 'target_fn', and optional location expressions.
#' @return Invisibly returns `TRUE` if all validations pass.
#' @noRd
.assert_footnote_specifications_list <- function(footnote_specs_list,
                                                 arg_name_for_msg,
                                                 data_colnames,
                                                 allow_null = TRUE) {
  if (is.null(footnote_specs_list) && isTRUE(allow_null)) {
    return(invisible(TRUE))
  }
  if (!is.list(footnote_specs_list) || is.data.frame(footnote_specs_list)) {
    stop(sprintf("Argument '%s' must be a list.", arg_name_for_msg), call. = FALSE)
  }

  if (length(footnote_specs_list) == 0 || (is.character(footnote_specs_list) && footnote_specs_list == "default")) {
    return(invisible(TRUE))
  }

  for (i in seq_along(footnote_specs_list)) {
    spec <- footnote_specs_list[[i]]
    current_spec_desc <- sprintf("element #%d in '%s'", i, arg_name_for_msg)

    if (!is.list(spec) || is.data.frame(spec)) {
      stop(sprintf("%s must be a list.", current_spec_desc), call. = FALSE)
    }

    if (is.null(spec$text) || !is.character(spec$text) || length(spec$text) != 1 || spec$text == "") {
      stop(sprintf("'text' in %s must be a single, non-empty character string.", current_spec_desc), call. = FALSE)
    }

    if (is.null(spec$target_fn) || !is.function(spec$target_fn)) {
      stop(sprintf(
        "'target_fn' in %s must be a function (e.g., gt::cells_body). \nReceived type: %s.",
        current_spec_desc, typeof(spec$target_fn)
      ), call. = FALSE)
    }

    if (!is.null(spec$columns_expr) && !(rlang::is_expression(spec$columns_expr) || is.call(spec$columns_expr) || is.symbol(spec$columns_expr))) {
      stop(sprintf(
        "'columns_expr' in %s, if provided, must be a column name (symbol), or a tidyselect call. \nReceived type: %s.",
        current_spec_desc, typeof(spec$columns_expr)
      ), call. = FALSE)
    }

    if (!is.null(spec$rows_expr)) {
      if (!(rlang::is_expression(spec$rows_expr) || is.call(spec$rows_expr) ||
        is.symbol(spec$rows_expr) || is.character(spec$rows_expr) ||
        is.numeric(spec$rows_expr) || is.logical(spec$rows_expr))) {
        stop(sprintf(
          "'rows_expr' in %s, if provided, must be a row identifier (e.g., numeric, logical) or an expression. \nReceived type: %s.",
          current_spec_desc, typeof(spec$rows_expr)
        ), call. = FALSE)
      }
    }
  }
  invisible(TRUE)
}


#' Validate Ellipsis Arguments for calculate_reliability
#'
#' @description
#' An internal validator specifically for the `...` argument in the
#' `calculate_reliability` function.
#' @return Invisibly returns `TRUE` if validation passes.
#' @noRd
.validate_reliability_ellipsis <- function(ellipsis_args, arg_name_for_msg, data) {
  psych_alpha_arg_names <- intersect(names(formals(psych::alpha)), names(ellipsis_args))
  items_list_args <- ellipsis_args[setdiff(names(ellipsis_args), psych_alpha_arg_names)]

  if (length(items_list_args) == 0) {
    stop("No scales provided in `...`. Please provide at least one named argument with a character vector of item names.", call. = FALSE)
  }

  for (scale_name in names(items_list_args)) {
    items <- items_list_args[[scale_name]]
    .assert_argument_is_character_vector(items, paste0("scale '", scale_name, "'"), allow_null = FALSE)

    missing_cols <- setdiff(items, names(data))
    if (length(missing_cols) > 0) {
      stop(sprintf("Item(s) not found in data for scale '%s':\n%s", scale_name, paste(dQuote(missing_cols), collapse = ", ")), call. = FALSE)
    }
  }

  invisible(TRUE)
}


#' @title Assert that an Argument is a Named List
#' @description Checks if an argument is a list with names. It specifically
#'   allows an empty list `list()` to pass validation.
#' @return Invisibly returns `NULL` if validation passes.
#' @noRd
.assert_argument_is_named_list <- function(arg_value,
                                           arg_name_for_msg,
                                           allow_null = TRUE) {
  # Case 1: NULL is allowed and the value is NULL.
  if (allow_null && is.null(arg_value)) {
    return(invisible(NULL))
  }

  # Case 2: The value is not a list at all.
  if (!is.list(arg_value)) {
    stop(sprintf("Argument '%s' must be a list.", arg_name_for_msg), call. = FALSE)
  }

  # Case 3: The list is not empty, but has no names or empty names.
  # An empty list `list()` has length 0 and is considered valid.
  if (length(arg_value) > 0 && (is.null(names(arg_value)) || any(names(arg_value) == ""))) {
    stop(sprintf("Argument '%s' must be a named list with non-empty names.", arg_name_for_msg), call. = FALSE)
  }

  invisible(NULL)
}


#' @title Assert that a Character Vector Contains a Specific Set of Strings
#' @description Checks if a character vector contains exactly the expected strings.
#'   Allows for an empty vector if `allow_null` is TRUE.
#' @return Invisibly returns `NULL` if validation passes.
#' @noRd
.assert_vector_contains_all_expected_strings <- function(arg_value,
                                                         expected_strings,
                                                         arg_name_for_msg,
                                                         allow_null = TRUE) {
  # Case 1: NULL is allowed and the value is NULL.
  if (allow_null && is.null(arg_value)) {
    return(invisible(NULL))
  }

  # Case 2: An empty vector is a valid "not set" state if NULL is allowed.
  if (allow_null && length(arg_value) == 0) {
    return(invisible(NULL))
  }

  # First, ensure it's a character vector.
  .assert_argument_is_character_vector(arg_value, arg_name_for_msg, allow_null = FALSE)

  # Now, for non-empty vectors, check for exact set equality.
  if (!setequal(arg_value, expected_strings)) {
    stop(sprintf(
      "Argument '%s' must contain exactly the values: %s. Received: %s.",
      arg_name_for_msg,
      paste(dQuote(expected_strings), collapse = ", "),
      paste(dQuote(arg_value), collapse = ", ")
    ), call. = FALSE)
  }

  invisible(NULL)
}


#' @title Assert that the Names of a List are in an Allowed Set
#' @description Checks if all names of a named list are present in a given set
#'   of allowed strings. Upon failure, it stops with a detailed error message
#'   that shows the invalid names, the user's input, and the allowed values.
#'
#' @param arg_value The named list to check.
#' @param allowed_strings A character vector of valid names.
#' @param arg_name_for_msg The name of the argument being checked, for use in
#'   the error message.
#' @param allow_null A logical value. If `TRUE`, a `NULL` `arg_value` will pass
#'   the validation.
#'
#' @return Invisibly returns `NULL` if validation passes.
#' @keywords internal
#' @noRd
.assert_names_are_in_allowed_set <- function(arg_value,
                                             allowed_strings,
                                             arg_name_for_msg,
                                             allow_null = TRUE) {
  # Handle NULL case
  if (allow_null && is.null(arg_value)) {
    return(invisible(NULL))
  }

  # Basic type check
  .assert_argument_is_named_list(arg_value, arg_name_for_msg, allow_null = FALSE)

  # --- Main Validation Logic ---
  user_names <- names(arg_value)
  invalid_names <- setdiff(user_names, allowed_strings)

  # If there are no invalid names, the check passes.
  if (length(invalid_names) == 0) {
    return(invisible(NULL))
  }

  # --- Error Message Construction (if check fails) ---

  # Helper to format a vector for clean printing in the error message
  format_vector_for_msg <- function(vec, max_items = 10) {
    if (length(vec) > max_items) {
      vec <- c(head(vec, max_items), "...")
    }
    paste(dQuote(vec), collapse = ", ")
  }

  # Construct the detailed error message
  error_msg <- sprintf(
    "Argument '%s' contains invalid names: %s.\n  Please check the variable names in your model.\n\n  You gave: %s\n  Allowed: %s",
    arg_name_for_msg,
    format_vector_for_msg(invalid_names),
    format_vector_for_msg(user_names),
    format_vector_for_msg(allowed_strings)
  )

  stop(error_msg, call. = FALSE)
}
