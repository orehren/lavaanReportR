#' @title Sanitize a String for Use as a DOT ID
#' @description Replaces all non-alphanumeric characters (except underscore)
#'   with an underscore to create a valid Graphviz node/cluster ID.
#' @param str The input character string. Can be a vector.
#' @return A sanitized character string vector.
#' @keywords internal
#' @noRd
.sanitize_string <- function(str) {
  # Return NA for NA inputs to avoid errors
  if (is.null(str) || all(is.na(str)) || all(str == "")) {
    return(NA_character_)
  }
  gsub("[^a-zA-Z0-9_]", "_", str)
}
