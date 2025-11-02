# ==============================================================================
# SECTION: GLOBAL PACKAGE CONSTANTS
# This file defines constants used throughout the lavaanReportR package to
# ensure consistency and avoid magic strings.
# ==============================================================================

#' @title Model Operator Labels
#' @description A named list mapping internal keys to the operator labels as they
#'   appear in the `op` column of the parameter table.
#' @keywords internal
#' @noRd
MODEL_OPS <- list(
  LOADINGS = "Latent Variable",
  REGRESSIONS = "Regression Path",
  COVARIANCES = "Covariance",
  VARIANCES = "Variance",
  DEFINED = "Defined Parameter",
  INTERCEPTS = "Intercept"
)

#' @title Semantic Type Keys
#' @description Defines consistent keys for the primary semantic meaning of a
#'   parameter table row.
#' @keywords internal
#' @noRd
SEMANTIC_TYPES <- list(
  LOADING = "loading",
  MODERATION = "moderation_effect",
  REGRESSION = "regression",
  COVARIANCE = "covariance",
  VARIANCE = "variance",
  INTERCEPT = "intercept",
  INDIRECT = "indirect_effect",
  TOTAL = "total_effect",
  CONTRAST = "contrast",
  DIRECT = "direct_effect"
)

#' @title Node Type Keys
#' @description Defines consistent keys for different types of nodes in the graph.
#' @keywords internal
#' @noRd
NODE_TYPES <- list(
  LATENT = "latent",
  MANIFEST = "manifest",
  VARIANCE = "variance",
  INTERCEPT = "intercept",
  MODERATOR = "moderator",
  ANCHOR_ADJ = "anchor_adj",
  ANCHOR_PATH = "anchor_path"
)

#' @title Edge Type Keys
#' @description Defines consistent keys for different types of edges in the graph.
#' @keywords internal
#' @noRd
EDGE_TYPES <- list(
  LOADING = "loading",
  REGRESSION = "regression",
  COVARIANCE = "covariance",
  # COVARIANCE_ENDO = "covariance_endo",
  VARIANCE = "variance_link",
  INTERCEPT = "intercept_link",
  MODERATION = "moderation",
  MODERATED_PATH_SEGMENT_1 = "moderated_path_segment_1",
  MODERATED_PATH_SEGMENT_2 = "moderated_path_segment_2",
  DEFINED = "defined_link"
)

#' @title Text Element Groups for Styling
#' @description This list defines the groups of elements that can be targeted
#'   by text size arguments.
#' @keywords internal
#' @noRd
TEXT_ELEMENT_GROUPS <- list(
  nodes = c("manifest", "latent", "intercept", "variance"),
  edges = c("regression", "loading", "covariance", "indirect")
)

#' @title Text Size Argument Hierarchy
#' @description This table defines the hierarchy and behavior of the `text_size_*`
#'   arguments. It is the single source of truth for the fontsize override logic.
#' @keywords internal
#' @noRd
TEXT_SIZE_RULE_MAP <- data.table::data.table(
  priority = c(1, 2, 2, 3),
  arg_name = c("text_size_global", "text_size_nodes", "text_size_edges", "text_size_details"),
  target_keys = list(
    unlist(TEXT_ELEMENT_GROUPS),
    TEXT_ELEMENT_GROUPS$nodes,
    TEXT_ELEMENT_GROUPS$edges,
    NULL # Special flag for the 'details' case
  )
)


#' @title Argument Normalization Map
#' @description This list serves as a configuration map for the
#'   `.normalize_user_args` function. It defines which user arguments should be
#'   normalized using `match.arg` and specifies the valid choices and behavior
#'   for each.
#'
#'   Each entry in the list should be another list with two elements:
#'   - `$choices`: A character vector of the full, allowed argument values.
#'   - `$several.ok`: A logical value. If `TRUE`, the argument can be a vector
#'     of multiple choices. If `FALSE`, only a single value is allowed.
#'
#' @keywords internal
#' @noRd
NORMALIZATION_MAP <- list(
  estimates_to_show = list(
    choices = validation_rules$estimates_to_show$args$allowed_strings,
    several.ok = FALSE
  ),
  plot_flow_direction = list(
    choices = validation_rules$plot_flow_direction$args$allowed_strings,
    several.ok = FALSE
  ),
  show_plot_elements = list(
    choices = validation_rules$show_plot_elements$args$allowed_strings, # c("intercepts", "variances", "covariances"),
    several.ok = TRUE
  ),
  multilevel_multigroup_order = list(
    choices = validation_rules$multilevel_multigroup_order$args$allowed_strings, # c("group", "level"),
    several.ok = TRUE
  )
)

#' @title Default Argument Map
#' @description This list provides a complete, type-stable set of default
#'   values for all user-configurable arguments. It is the single source of
#'   truth for default behaviors.
#' @keywords internal
#' @noRd
DEFAULT_ARGS_MAP <- list(
  estimates_to_show = "standardized",
  recipe = list(),
  show_plot_elements = character(0),
  plot_flow_direction = character(0),
  show_sig = TRUE,
  show_estimates_above = 0,
  text_size_global = NA_real_,
  text_size_nodes = NA_real_,
  text_size_edges = NA_real_,
  text_size_details = list(),
  show_legend = TRUE,
  show_legend_with_effects = TRUE,
  multilevel_multigroup_order = c("group", "level"),
  node_labels = list(),
  effect_labels = list(),
  render = TRUE
)


#' @title Default Node Style Attributes
#' @description A named list defining the default visual attributes (shape, size, etc.)
#'   for each semantic node type. This serves as the base layer for styling,
#'   which can be overridden by the recipe.
#' @keywords internal
#' @noRd
NODE_STYLES <- list(
  latent = list(shape = "ellipse", width = 0.35, height = 0.25),
  manifest = list(shape = "box"),
  variance = list(shape = "circle"),
  intercept = list(shape = "triangle"),
  moderator = list(shape = "diamond"),
  anchor_path = list(shape = "point", style = "invis", width = 0, height = 0, margin = 0), # ev. noch shape = plaintext , fixedsize
  anchor_adj = list(shape = "point", style = "invis", width = 0, height = 0)
)

EDGE_STYLES <- list(
  regression = list(minlen = 3), # style = "solid", arrowhead = "normal", penwidth = 1.5,
  loading = list(minlen = 3), # style = "solid", arrowhead = "normal", penwidth = 1.5,
  moderation = list(arrowhead = "normal"), # style = "solid", penwidth = 1.5
  moderated_path_segment_1 = list(arrowhead = "none"), # style = "solid", penwidth = 1.5
  moderated_path_segment_2 = list(minlen = 1), # style = "solid", arrowhead = "normal", penwidth = 1.5
  covariance = list(style = "dotted", arrowhead = "none", constraint = "false", fontsize = 8), # , penwidth = 1.5,
  # covariance_exo = list(style = "dashed", dir = "both", arrowhead = "none", penwidth = 1.5, fontsize = 8),
  indirect = list(style = "dashed", penwidth = 2, color = "grey40"), #  arrowhead = "normal",
  variance_link = list(style = "dotted", arrowhead = "none"),
  intercept_link = list(style = "solid", arrowhead = "none", penwidth = 0.5)
)

#' @title Declarative Rules for Node Extraction
#' @description This constant is a named list that serves as a declarative
#'   "recipe book" for the node analysis phase. Each element in the list is a
#'   "rule" that defines how to identify and construct a specific semantic node
#'   type (e.g., latent, variance, moderator) from the `primary_nodes` source
#'   table.
#'
#' @details
#' The `.analyze_node_structure` function iterates over this list, passing each
#' rule to the `.build_derived_nodes` factory function. This design pattern
#' separates the "what" (the rules defined here) from the "how" (the execution
#' logic in the factory), making the system highly modular and extensible.
#'
#' Each rule is a `list` and must contain the following named elements:
#' \describe{
#'   \item{`filter_expr`}{A `quote()`-d R expression. This expression is used in
#'     the `i` slot of a `data.table` query to filter the `primary_nodes` table
#'     and select only the rows relevant to this node type. It can refer to
#'     columns in `primary_nodes` like `op` and `variable`.}
#'   \item{`id_expr`}{A `quote()`-d R expression that defines how to construct
#'     the unique `id` for each node of this type. This often involves pasting
#'     a suffix (e.g., "_var") to the base variable name.}
#'   \item{`unit_expr`}{A `quote()`-d R expression that defines the `node_unit`.
#'     This column stores the original, unmodified variable name the node is
#'     associated with, which is crucial for grouping related nodes (e.g., a
#'     variable, its variance, and its intercept).}
#'   \item{`node_type`}{A static character string from the `NODE_TYPES` constant
#'     that assigns the semantic type to all nodes created by this rule.}
#' }
#'
#' @format A list of lists, where each inner list is a node extraction rule.
#'
#' @keywords internal
#' @noRd
NODE_EXTRACTION_RULES <- list(
  list(
    filter_expr = quote(op == MODEL_OPS$LOADINGS),
    id_expr = quote(variable),
    unit_expr = quote(variable),
    node_type = NODE_TYPES$LATENT,
    id_suffix = NULL
  ),
  list(
    # This is a broad rule to ensure that any variable with a variance
    # gets a main node. It runs after the LATENT rule, and the final de-duplication
    # ensures that latent variables are not overwritten.
    filter_expr = quote(op == MODEL_OPS$VARIANCES),
    id_expr = quote(variable),
    unit_expr = quote(variable),
    node_type = NODE_TYPES$MANIFEST,
    id_suffix = NULL
  ),
  list(
    filter_expr = quote(op == MODEL_OPS$REGRESSIONS & !variable %like% ":"),
    id_expr = quote(variable),
    unit_expr = quote(variable),
    node_type = NODE_TYPES$MANIFEST,
    id_suffix = NULL
  ),
  list(
    filter_expr = quote(op == MODEL_OPS$VARIANCES),
    id_expr = quote(variable),
    unit_expr = quote(variable),
    node_type = NODE_TYPES$VARIANCE,
    id_suffix = "var"
  ),
  list(
    filter_expr = quote(op == MODEL_OPS$INTERCEPTS),
    id_expr = quote(variable),
    unit_expr = quote(variable),
    node_type = NODE_TYPES$INTERCEPT,
    id_suffix = "int"
  ),
  list(
    filter_expr = quote(op == MODEL_OPS$REGRESSIONS & variable %like% ":"),
    id_expr = quote(.sanitize_string(variable)),
    unit_expr = quote(variable),
    node_type = NODE_TYPES$MODERATOR,
    id_suffix = NULL
  ),
  list(
    filter_expr = quote(op == MODEL_OPS$REGRESSIONS & variable %like% ":"),
    id_expr = quote(.sanitize_string(variable)),
    unit_expr = quote(variable),
    node_type = NODE_TYPES$ANCHOR_PATH,
    id_suffix = "path"
  )
)


#' @title Declarative Rules for Initial Edge Extraction
#' @description This constant is a named list that serves as a declarative
#'   "recipe book" for the initial edge analysis. Each element is a rule that
#'   defines how to identify and construct a basic edge type from the `param_table`.
#'
#' @details
#' The `.analyze_edge_structure` function iterates over this list, passing each
#' rule to the `.build_derived_edges` factory. This first pass extracts all
#' standard edges. A subsequent step then transforms these basic edges to handle
#' complex cases like moderation.
#'
#' Each rule is a `list` and must contain:
#' \describe{
#'   \item{`filter_expr`}{A `quote()`-d expression for filtering the `param_table`.}
#'   \item{`from_expr`}{A `quote()`-d expression defining the source (`from`) node of the edge.}
#'   \item{`to_expr`}{A `quote()`-d expression defining the target (`to`) node of the edge.}
#'   \item{`edge_type`}{A static character string from `EDGE_TYPES`.}
#'   \item{`id_prefix`}{A short, static character prefix for creating the edge ID.}
#' }
#'
#' @format A list of lists, where each inner list is an edge extraction rule.
#'
#' @keywords internal
#' @noRd
EDGE_EXTRACTION_RULES <- list(
  list(
    filter_expr = quote(op == MODEL_OPS$REGRESSIONS & !rhs %like% ":"),
    from_expr = quote(rhs),
    to_expr = quote(lhs),
    edge_type = EDGE_TYPES$REGRESSION,
    id_prefix = "reg"
  ),
  list(
    filter_expr = quote(op == MODEL_OPS$LOADINGS & !rhs %like% ":"),
    from_expr = quote(lhs),
    to_expr = quote(rhs),
    edge_type = EDGE_TYPES$LOADING,
    id_prefix = "load"
  ),
  list(
    filter_expr = quote(op == MODEL_OPS$COVARIANCES),
    from_expr = quote(lhs),
    to_expr = quote(rhs),
    edge_type = EDGE_TYPES$COVARIANCE,
    id_prefix = "cov"
  ),
  list(
    filter_expr = quote(op == MODEL_OPS$VARIANCES),
    from_expr = quote(lhs),
    to_expr = quote(paste0(.sanitize_string(lhs), "_var")),
    edge_type = EDGE_TYPES$VARIANCE,
    id_prefix = "var"
  ),
  list(
    filter_expr = quote(op == MODEL_OPS$INTERCEPTS),
    from_expr = quote(paste0(.sanitize_string(lhs), "_int")),
    to_expr = quote(lhs),
    edge_type = EDGE_TYPES$INTERCEPT,
    id_prefix = "int"
  ),
  list(
    # MODERATION segment 1: from first predictor to anchor
    filter_expr = quote(op == MODEL_OPS$REGRESSIONS & rhs %like% ":"),
    from_expr = quote(strsplit(rhs, ":")[[1]][1]),
    to_expr = quote(paste0(.sanitize_string(rhs), "_path")),
    edge_type = EDGE_TYPES$MODERATED_PATH_SEGMENT_1,
    id_prefix = "mod"
  ),
  list(
    # MODERATION segment 2: from anchor to outcome
    filter_expr = quote(op == MODEL_OPS$REGRESSIONS & rhs %like% ":"),
    from_expr = quote(paste0(.sanitize_string(rhs), "_path")),
    to_expr = quote(lhs),
    edge_type = EDGE_TYPES$MODERATED_PATH_SEGMENT_2,
    id_prefix = "mod"
  ),
  list(
    # MODERATION arrow: from anchor to moderator node
    filter_expr = quote(op == MODEL_OPS$REGRESSIONS & rhs %like% ":"),
    from_expr = quote(paste0(.sanitize_string(rhs), "_path")),
    to_expr = quote(.sanitize_string(rhs)),
    edge_type = EDGE_TYPES$MODERATION,
    id_prefix = "mod"
  )
)
