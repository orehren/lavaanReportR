# ==============================================================================
# SECTION: LAYOUT HELPERS
# ==============================================================================

# ------------------------------------------------------------------------------
# Helpers for Layout Analysis
# ------------------------------------------------------------------------------

#' @title Calculate Hierarchical Structure
#' @description The core layout algorithm using `igraph`. It determines the
#'   hierarchical levels of nodes in a directed acyclic graph.
#'
#' @param nodes A character vector of node IDs.
#' @param edges A data.table with `from` and `to` columns for directed edges.
#'
#' @return A named list where names are level numbers (as characters) and
#'   values are character vectors of the node IDs on that level.
#' @keywords internal
#' @noRd
.calculate_hierarchical_structure <- function(nodes, edges) {
  graph <- igraph::graph_from_data_frame(edges, directed = TRUE, vertices = nodes)

  if (!igraph::is_dag(graph)) {
    warning("A cycle was detected in the model graph. Hierarchical layout may be incorrect.", call. = FALSE)
    return(list(`1` = nodes))
  }

  # --- Core Algorithm ---

  # 1. Get a topologically sorted list of nodes
  sorted_nodes <- igraph::topo_sort(graph, mode = "out") |> names()

  # 2. Initialize the level vector for all nodes
  node_names <- igraph::V(graph) |> names()
  node_levels <- rep(1, length(node_names)) |> stats::setNames(node_names)

  # 3. Iterate backwards through the sorted list to calculate levels
  for (node in rev(sorted_nodes)) {
    successors <- igraph::neighbors(graph, node, mode = "out") |> names()

    # Guard Clause: If a node has no successors (it's a sink), its level
    # remains 1. Skip to the next node.
    if (length(successors) == 0) {
      next
    }

    # The level of a node is 1 + the maximum level of its children.
    node_levels[node] <- 1 + max(node_levels[successors])
  }

  # 4. Invert the levels so that source nodes start at level 1
  final_levels <- max(node_levels) - node_levels + 1

  # 5. Group nodes by their final level number
  final_levels |>
    names() |>
    split(f = final_levels)
}


#' @title Analyze the Structure of a Single Element Unit
#' @description Calculates the internal hierarchical structure for a single,
#'   pre-defined unit of elements (e.g., all "predictors" or all "growth_factors").
#'   It filters the relevant paths and calls the core layout algorithm.
#'
#' @param element_unit_name The name of the element unit (e.g., "predictors").
#' @param element_unit_nodes A list containing a character vector of node IDs in the unit.
#' @param all_paths A data.table of all directed paths in the model.
#'
#' @return A list containing the calculated `$levels_list` and a data.table
#'   with the `$element_unit_info`, or `NULL` if the unit is empty.
#' @keywords internal
#' @noRd
.analyze_element_unit_structure <- function(element_unit_name, element_unit_nodes, all_paths) {
  # Unpack the vector from the list structure provided by `purrr::map2`
  nodes <- unlist(element_unit_nodes)

  # Guard Clause: If this element unit contains no nodes, there is nothing to do.
  if (is.null(nodes) || length(nodes) == 0) {
    return(NULL)
  }

  # Filter the global path list to get only the edges internal to this unit.
  internal_edges <- all_paths[from %in% nodes & to %in% nodes]

  # Call the core algorithm to calculate the hierarchical levels for this unit.
  levels_list <- .calculate_hierarchical_structure(nodes, internal_edges)

  # Assemble the final output for this unit.
  list(
    levels_list = levels_list,
    element_unit_info = data.table::data.table(id = nodes, element_unit = element_unit_name)
  )
}


#' @title Collect All Directed Paths for Layout
#' @description Extracts all edges that define the hierarchical structure of the
#'   graph. This now correctly includes the segments of moderated paths.
#' @param edges The data.table of all extracted edges from `.analyze_edge_structure`.
#' @return A data.table with `from` and `to` columns.
#' @keywords internal
.collect_all_directed_paths <- function(edges) {
  directed_edge_types <- c(
    EDGE_TYPES$REGRESSION,
    EDGE_TYPES$LOADING,
    EDGE_TYPES$MODERATION,
    EDGE_TYPES$MODERATED_PATH_SEGMENT_1,
    EDGE_TYPES$MODERATED_PATH_SEGMENT_2
  )

  edges[edge_type %in% directed_edge_types, .(from, to)]
}


#' @title Assemble Final Layout Data
#' @description Combines the layout results from multiple element units into a
#'   single, final layout object containing the complete level list and a map
#'   of nodes to their element units.
#'
#' @param layout_results A list of results from `.analyze_element_unit_structure`.
#'
#' @return A list containing the final `$levels` (a named list) and
#'   `$element_unit_map` (a data.table).
#' @keywords internal
#' @noRd
.assemble_layout_data <- function(layout_results) {
  # Filter out any NULL results from element units that had no nodes
  valid_results <- compact(layout_results)

  # Guard Clause: If no valid layouts could be calculated at all.
  if (length(valid_results) == 0) {
    warning("Could not determine layout; no valid element units found.", call. = FALSE)
    return(list(levels = list(), element_unit_map = data.table::data.table()))
  }

  # --- 1. Assemble the final list of levels ---

  # Combine all level lists from the valid results into a single list.
  # `unlist(recursive = FALSE)` correctly merges the lists of lists.
  all_levels <- map(valid_results, "levels_list") |>
    unlist(recursive = FALSE)

  # Re-number the final levels sequentially from 1.
  names(all_levels) <- seq_along(all_levels)

  # --- 2. Assemble the final map of elements to units ---

  # Combine all element unit info tables into a single map.
  element_unit_map <- map_dfr(valid_results, "element_unit_info", .id = "element_unit_order")

  # Convert the order column to integer for potential sorting later.
  element_unit_map[, element_unit_order := as.integer(element_unit_order)]

  # --- 3. Return the final assembled object ---
  list(levels = all_levels, element_unit_map = element_unit_map)
}


#' @title Analyze Graph Layout (Orchestrator)
#' @description Orchestrates the calculation of hierarchical layout levels for
#'   all nodes in the model. This version uses a robust, two-step approach
#'   that correctly consolidates ranks for all semantic node units.
#'
#' @param nodes The data.table of all extracted nodes.
#' @param edges The data.table of all extracted edges.
#'
#' @return A named list containing the final layout data (`$levels`).
#' @keywords internal
#' @noRd
.analyze_layout_structure <- function(nodes, edges) {
  # --- 1. & 2. Initial Rank Calculation (bleibt unverändert) ---
  all_node_ids <- nodes$id
  directed_paths <- .collect_all_directed_paths(edges)
  unique_node_ids <- unique(all_node_ids)

  graph <- igraph::graph_from_data_frame(directed_paths, directed = TRUE, vertices = unique_node_ids)

  if (!igraph::is_dag(graph)) {
    warning("A cycle was detected...", call. = FALSE)
    return(list(levels = list(`1` = unique_node_ids), element_unit_map = data.table()))
  }

  sorted_nodes <- names(igraph::topo_sort(graph, mode = "out"))
  node_levels <- stats::setNames(rep(1, length(unique_node_ids)), unique_node_ids)

  for (node in rev(sorted_nodes)) {
    successors <- names(igraph::neighbors(graph, node, mode = "out"))
    if (length(successors) > 0) {
      node_levels[node] <- 1 + max(node_levels[successors])
    }
  }
  initial_ranks <- max(node_levels) - node_levels + 1

  # --- 3. Consolidate Ranks by Semantic Node Unit (Radically Simplified) ---

  # 3a. Create a map of each node to its initial rank and its unit.
  node_rank_map <- data.table(id = names(initial_ranks), initial_rank = initial_ranks)
  node_rank_map[nodes, on = "id", node_unit := i.node_unit]

  # 3b. Create a single, definitive lookup table for the "master" rank of each unit.
  #     The master rank is simply the rank of the main node (id == sanitized unit).
  #     This single rule now applies universally to all node types.
  master_ranks <- node_rank_map[
    id == fcase(
      node_unit %like% ":", paste0(.sanitize_string(node_unit), "_path"),
      default = .sanitize_string(node_unit)
    ),
    .(node_unit, master_rank = initial_rank)
  ]

  # 3c. Apply the determined master rank to all nodes within that unit.
  node_rank_map[master_ranks, on = "node_unit", final_rank := i.master_rank]

  # --- 4. Assemble the final layout list ---
  final_levels <- split(node_rank_map$id, f = node_rank_map$final_rank)

  return(list(levels = final_levels, element_unit_map = data.table()))
}

.calculate_xy_layout <- function(nodes, edges) {
  # 1. Determine hierarchical ranks
  ranks <- .analyze_layout_structure(nodes, edges)
  directed_paths <- .collect_all_directed_paths(edges)

  # Convert ranks to a data.table for easier manipulation
  layout_dt <- data.table(
    id = unlist(ranks$levels, use.names = FALSE),
    rank = as.numeric(rep(names(ranks$levels), lengths(ranks$levels)))
  )

  # 2. Assign initial deterministic x/y coordinates
  median_rank <- median(unique(layout_dt$rank))
  layout_dt[, y := -(rank - median_rank)]
  setorder(layout_dt, y, id)
  layout_dt[, x := seq(-.N/2 + 0.5, .N/2 - 0.5, by = 1), by = y]

  # 3. Iteratively refine x-coordinates using barycenter method
  n_iterations <- 3
  for (i in seq_len(n_iterations)) {

    # Create a lookup for current x positions
    pos_lookup <- setNames(layout_dt$x, layout_dt$id)

    # Get parent and child positions
    parents <- directed_paths[, .(parent_x = pos_lookup[from]), by = .(to = to)]
    children <- directed_paths[, .(child_x = pos_lookup[to]), by = .(from = from)]

    # Calculate barycenters
    bary_parents <- parents[, .(barycenter = mean(parent_x, na.rm = TRUE)), by = .(id = to)]
    bary_children <- children[, .(barycenter = mean(child_x, na.rm = TRUE)), by = .(id = from)]

    # Merge barycenters, giving precedence to parent-based positioning
    barycenters <- rbind(bary_parents, bary_children[!id %in% bary_parents$id])

    # Update desired x-positions in the layout table
    layout_dt[barycenters, on = "id", desired_x := i.barycenter]

    # Re-sort and re-assign final x to avoid overlaps
    setorder(layout_dt, y, desired_x, na.last = TRUE)
    layout_dt[, x := seq(-.N/2 + 0.5, .N/2 - 0.5, by = 1), by = y]
  }

  return(layout_dt[, .(id, x, y)])
}


#' @title Calculate Layout for a Plot
#' @description This is the third phase of the `lavaanReportR` workflow. The
#'   `layout` function takes a `lavaan_plot_config` object and calculates the
#'   visual layout of the graph.
#' @param x An object of class \code{lavaan_plot_config}.
#' @param ... Additional arguments (not used).
#' @return A \code{lavaan_layout} object containing the calculated layout information.
#' @exportS3Method lavaanReportR::layout
layout.lavaan_plot_config <- function(x, ...) {
  config <- x
  analyzed_model <- config$analyzed_model

  # layout <- .analyze_layout_structure(analyzed_model$nodes, analyzed_model$edges)
  layout <- .calculate_xy_layout(analyzed_model$nodes, analyzed_model$edges)

  .new_lavaan_layout(
    config = config,
    layout = layout
  )
}
