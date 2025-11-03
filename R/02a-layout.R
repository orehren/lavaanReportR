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
.analyze_layout_structure <- function(nodes, edges, element_groups, manual_ranks = NULL) {
  # --- 1. & 2. Initial Rank Calculation (bleibt unverändert) ---
  all_node_ids <- nodes$id
  directed_paths <- .collect_all_directed_paths(edges)

  # CRITICAL FIX: Ensure all nodes are passed to the graph constructor.
  # This includes nodes that may only be connected by non-directed paths
  # (e.g., covariances) or are isolates, ensuring they are part of the
  # layout process.
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

  # --- 3. Apply Manual Rank Overrides ---
  if (!is.null(manual_ranks)) {
    for (group_name in names(manual_ranks)) {
      if (group_name %in% names(element_groups)) {
        nodes_in_group <- element_groups[[group_name]]
        initial_ranks[nodes_in_group] <- manual_ranks[[group_name]]
      }
    }
  }

  # --- 3b. Propagate Rank Changes ---
  # After a manual override, the ranks of downstream nodes may need to be
  # adjusted to maintain the topological order (parents must have a lower
  # rank number than their children).
  propagated_ranks <- initial_ranks
  for (node in sorted_nodes) {
    parents <- names(igraph::neighbors(graph, node, mode = "in"))
    if (length(parents) > 0) {
      max_parent_rank <- max(propagated_ranks[parents])
      if (propagated_ranks[node] <= max_parent_rank) {
        propagated_ranks[node] <- max_parent_rank + 1
      }
    }
  }
  initial_ranks <- propagated_ranks

  # --- 4. Consolidate Ranks by Semantic Node Unit (Radically Simplified) ---

  # 4a. Create a map of each node to its initial rank and its unit.
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

.calculate_xy_layout <- function(nodes, edges, element_groups, manual_ranks = NULL, flow_direction = "LR") {
  # --- 1. Initial Setup ---
  ranks <- .analyze_layout_structure(nodes, edges, element_groups, manual_ranks)

  layout_dt <- data.table(
    id = unlist(ranks$levels, use.names = FALSE),
    rank = as.numeric(rep(names(ranks$levels), lengths(ranks$levels)))
  )

  # Merge with original node data to get type and unit info
  layout_dt[nodes, on = "id", `:=`(
    node_type = i.node_type,
    node_unit = i.node_unit
  )]

  # --- 2. Primary Axis Coordinate Calculation (Grid Logic) ---
  n_ranks <- data.table::uniqueN(layout_dt$rank)
  median_rank <- (1 + n_ranks) / 2
  section_size <- 3 # As specified

  # Assign primary coordinate based on rank and flow direction
  if (flow_direction == "LR") {
    layout_dt[, x := (rank - median_rank) * section_size]
    primary_axis <- "x"
    secondary_axis <- "y"
  } else { # TB
    layout_dt[, y := -(rank - median_rank) * section_size]
    primary_axis <- "y"
    secondary_axis <- "x"
  }

  # --- 3. Secondary Axis Coordinate Calculation (Within-Rank) ---

  # 3a. Barycenter Sorting
  directed_paths <- .collect_all_directed_paths(edges)
  pos_lookup <- setNames(layout_dt[[primary_axis]], layout_dt$id)

  neighbors <- rbind(
    directed_paths[, .(neighbor_pos = pos_lookup[from]), by = .(id = to)],
    directed_paths[, .(neighbor_pos = pos_lookup[to]), by = .(id = from)]
  )
  barycenters <- neighbors[, .(barycenter = mean(neighbor_pos, na.rm = TRUE)), by = id]

  layout_dt[barycenters, on = "id", barycenter := i.barycenter]

  # 3b. Cluster Sizing and Positioning
  # Calculate how many satellites each main node has
  layout_dt[node_type %in% c("variance", "intercept"), satellite_count := .N, by = node_unit]
  main_nodes <- layout_dt[node_type == "manifest" | node_type == "latent" | node_type == "moderator"]
  setorder(main_nodes, rank, barycenter)

  # Assign evenly spaced positions to the main nodes (clusters)
  main_nodes[, (secondary_axis) := seq(-.N/2 + 0.5, .N/2 - 0.5, by = 1) * 2, by = rank]

  # Merge the calculated secondary positions back into the main layout table
  layout_dt[main_nodes, on = "id", (secondary_axis) := mget(paste0("i.", secondary_axis))]

  # CRITICAL: Sort by unit and a custom factor to ensure the main node
  # (which has the coordinate) is first in each group before filling down.
  layout_dt[, sort_order := fcase(
    node_type %in% c("manifest", "latent", "moderator"), 1,
    default = 2
  )]
  setorder(layout_dt, node_unit, sort_order)
  layout_dt[, sort_order := NULL]

  # Fill down the secondary coordinate for satellite nodes from their main node
  layout_dt[, (secondary_axis) := zoo::na.locf(.SD[[1]], na.rm = FALSE), by = node_unit, .SDcols = secondary_axis]

  # --- 4. Smart Satellite Node Placement ---

  # 4a. Determine placement strategy for each main node unit.
  main_node_ids <- layout_dt[node_type %in% c("manifest", "latent", "moderator"), id]

  # For the "flow_direction" rule, the offset must be negative for TB layouts
  # because the y-coordinates decrease from top to bottom.
  flow_dir_multiplier <- if (flow_direction == "TB") -1 else 1

  placement_guides <- layout_dt[id %in% main_node_ids, {

    current_node_id <- .BY$id
    current_node_type <- layout_dt[id == current_node_id, node_type]

    children <- directed_paths[from == current_node_id, to]
    parents <- directed_paths[to == current_node_id, from]

    # SPECIAL CASE for Moderators: For layout, their "child" is their
    # anchor path, which is their parent in the directed graph. We re-assign
    # it here so the "look_ahead" logic is applied correctly.
    if (current_node_type == "moderator") {
      children <- parents
      parents <- character(0)
    }

    if (length(children) > 0) {
      # Rule 1: "look_ahead"
      child_coords <- layout_dt[id %in% children]
      main_node_coord <- layout_dt[id == current_node_id]

      avg_child_pos <- mean(child_coords[[secondary_axis]], na.rm = TRUE)
      main_node_pos <- main_node_coord[[secondary_axis]]

      direction <- ifelse(avg_child_pos > main_node_pos, -1, 1)
      list(placement_type = "offset_secondary", direction = direction)

    } else if (length(parents) > 0) {
      # Rule 2: "flow_direction"
      list(placement_type = "offset_primary", direction = flow_dir_multiplier)

    } else {
      # Rule 3: "default"
      list(placement_type = "default", direction = 1)
    }
  }, by = id]

  # 4b. Apply the calculated placement logic.
  units_to_place <- unique(layout_dt[node_type %in% c("variance", "intercept"), node_unit])

  offset_dist <- 0.5
  side_by_side_dist <- 0.25

  for (unit in units_to_place) {

    main_node_id <- .sanitize_string(unit)
    guide <- placement_guides[id == main_node_id]
    if (nrow(guide) == 0) next

    main_node <- layout_dt[id == main_node_id]
    var_idx <- which(layout_dt$node_unit == unit & layout_dt$node_type == "variance")
    int_idx <- which(layout_dt$node_unit == unit & layout_dt$node_type == "intercept")

    if (guide$placement_type == "offset_secondary") {
      new_secondary_pos <- main_node[[secondary_axis]] + (guide$direction * offset_dist)
      set(layout_dt, var_idx, j = secondary_axis, value = new_secondary_pos)
      set(layout_dt, int_idx, j = secondary_axis, value = new_secondary_pos)
      set(layout_dt, var_idx, j = primary_axis, value = main_node[[primary_axis]] - side_by_side_dist)
      set(layout_dt, int_idx, j = primary_axis, value = main_node[[primary_axis]] + side_by_side_dist)

    } else if (guide$placement_type == "offset_primary") {
      new_primary_pos <- main_node[[primary_axis]] + (guide$direction * offset_dist)
      set(layout_dt, var_idx, j = primary_axis, value = new_primary_pos)
      set(layout_dt, int_idx, j = primary_axis, value = new_primary_pos)
      set(layout_dt, var_idx, j = secondary_axis, value = main_node[[secondary_axis]] - side_by_side_dist)
      set(layout_dt, int_idx, j = secondary_axis, value = main_node[[secondary_axis]] + side_by_side_dist)

    } else { # "default"
      set(layout_dt, var_idx, j = secondary_axis, value = main_node[[secondary_axis]] + offset_dist)
      set(layout_dt, int_idx, j = secondary_axis, value = main_node[[secondary_axis]] - offset_dist)
      set(layout_dt, var_idx, j = primary_axis, value = main_node[[primary_axis]] - side_by_side_dist)
      set(layout_dt, int_idx, j = primary_axis, value = main_node[[primary_axis]] + side_by_side_dist)
    }
  }

  # --- 5. Anchor Path Node Placement ---
  anchor_paths <- layout_dt[node_type == "anchor_path"]
  if (nrow(anchor_paths) > 0) {
    # Find the two nodes connected by each anchor path
    # Segment 1: from -> anchor
    # Segment 2: anchor -> to
    seg1 <- edges[to %in% anchor_paths$id & edge_type == "moderated_path_segment_1", .(anchor_id = to, connected_node = from)]
    seg2 <- edges[from %in% anchor_paths$id & edge_type == "moderated_path_segment_2", .(anchor_id = from, connected_node = to)]

    connected_nodes <- rbind(seg1, seg2)

    # Get the coordinates of the connected nodes
    connected_nodes[layout_dt, on = .(connected_node = id), `:=`(x = i.x, y = i.y)]

    # Calculate the midpoint for each anchor
    midpoints <- connected_nodes[, .(x = mean(x), y = mean(y)), by = anchor_id]

    # Update the layout table with the anchor coordinates
    layout_dt[midpoints, on = .(id = anchor_id), `:=`(x = i.x, y = i.y)]
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
#' @importFrom data.table set
#' @exportS3Method lavaanReportR::layout
layout.lavaan_plot_config <- function(x, ...) {
  config <- x
  analyzed_model <- config$analyzed_model

  # Pass the rankdir from the recipe to the new flow_direction argument
  layout <- .calculate_xy_layout(
    nodes = analyzed_model$nodes,
    edges = analyzed_model$edges,
    element_groups = analyzed_model$features$element_groups,
    manual_ranks = config$recipe$manual_ranks,
    flow_direction = config$recipe$rankdir
  )

  .new_lavaan_layout(
    config = config,
    layout = layout
  )
}
