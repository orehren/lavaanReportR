# ==============================================================================
# SECTION: S3 METHOD FOR LAYOUT CALCULATION
# ==============================================================================

#' @title Calculate Node Layout Coordinates
#' @description An S3 generic for calculating the x/y coordinates for graph nodes.
#' @param object An object representing the graph to be laid out.
#' @param ... Additional arguments passed to specific methods.
#' @return A new object with layout information added.
#' @keywords internal
#' @noRd
layout <- function(object, ...) {
  UseMethod("layout")
}

#' @title Calculate Layout for a lavaan_graph Object
#' @description This is the main entry point for the layout phase. It takes a
#'   `lavaan_graph` object and calculates the precise x/y coordinates for each
#'   node using a custom, deterministic grid-based algorithm.
#' @param object A `lavaan_graph` object.
#' @param ... Additional arguments (not currently used).
#' @return A `lavaan_layout` object, which is the original `lavaan_graph` object
#'   with the calculated layout data (`$nodes_layout_coords`) added.
#' @keywords internal
#' @noRd
layout.lavaan_graph <- function(object, ...) {
  # 1. Call the core layout algorithm
  nodes_layout_coords <- .calculate_xy_layout(object$nodes, object$edges)

  # 2. Store the results in the object
  object$nodes_layout_coords <- nodes_layout_coords

  # 3. Create and return the new lavaan_layout object
  lavaan_layout(object)
}

# ==============================================================================
# SECTION: CORE LAYOUT ALGORITHM
# ==============================================================================

#' @title Calculate XY Layout Coordinates Using a Grid-Based Algorithm
#' @description This function implements the deterministic layout algorithm. It
#'   arranges nodes on a grid based on their topological rank (for the x-axis)
#'   and a barycenter-sorted position (for the y-axis) to minimize edge crossings.
#' @param nodes A `data.table` of all nodes in the graph.
#' @param edges A `data.table` of all edges in the graph.
#' @return A `data.table` with columns `id`, `x`, and `y` for each node.
#' @keywords internal
#' @noRd
.calculate_xy_layout <- function(nodes, edges) {
  # --- 1. Topological Ranking (for the Primary Axis) ---
  directed_paths <- edges[edge_type %in% c("regression", "loading"), .(from, to)]
  graph <- igraph::graph_from_data_frame(directed_paths, directed = TRUE, vertices = nodes$id)

  if (!igraph::is_dag(graph)) {
    warning("A cycle was detected in the model graph. Layout may be incorrect.", call. = FALSE)
    nodes[, rank := 1]
  } else {
    ranks <- igraph::topo_sort(graph, mode = "out") |> names() |> (\(x) stats::setNames(seq_along(x), x))()
    nodes[, rank := ranks[id]]
  }

  # --- 2. Grid & X-Coordinate Calculation ---
  # Each rank gets a fixed-width column on the grid.
  # The layout is centered around the median rank, which becomes x=0.
  median_rank <- stats::median(unique(nodes$rank), na.rm = TRUE)
  nodes[, x := (rank - median_rank) * 3]

  # --- 3. Secondary Axis (Y-Coordinate) Calculation ---
  # This is done rank by rank, using barycenter to sort nodes.
  nodes[, y := as.numeric(NA)]
  setorder(nodes, rank)

  # Pre-calculate barycenter values for all nodes
  edges_for_bary <- nodes[edges, on = .(id = to), .(from, x.to = i.x, y.to = i.y, x.from = x, y.from = y)]
  bary_lookup <- edges_for_bary[!is.na(y.from), .(barycenter = mean(y.from)), by = .(to = from)]

  # Iterate through each rank to set y-coordinates
  for (r in unique(nodes$rank)) {
    current_rank_nodes <- nodes[rank == r]

    # Get barycenter values for nodes in this rank
    current_rank_nodes[bary_lookup, on = .(id = to), barycenter := i.barycenter]

    # Sort main nodes by barycenter to minimize crossings
    # Use zoo::na.locf to propagate the last observed barycenter for initial ranks
    main_nodes <- current_rank_nodes[node_type %in% c("latent", "manifest")]
    main_nodes[, barycenter := zoo::na.locf(barycenter, na.rm = FALSE, fromLast = TRUE)]
    main_nodes[is.na(barycenter), barycenter := 0]
    setorder(main_nodes, barycenter)

    # Assign initial Y-positions to main nodes, creating space
    y_pos <- seq(from = 0, by = 2, length.out = nrow(main_nodes))
    y_pos <- y_pos - mean(y_pos) # Center them around y=0
    main_nodes[, y := y_pos]

    # Update the main nodes table with the new y-coordinates
    nodes[main_nodes, on = "id", y := i.y]

    # Place satellite nodes (variances, intercepts) relative to their main node
    satellite_nodes <- current_rank_nodes[!node_type %in% c("latent", "manifest")]
    satellite_nodes[nodes, on = .(node_unit = node_unit, node_type = "main_node_placeholder"), parent_y := i.y] # This join needs fixing

    # A better way to get parent_y
    parent_y_lookup <- nodes[node_type %in% c("latent", "manifest"), .(node_unit, parent_y = y)]
    satellite_nodes[parent_y_lookup, on = "node_unit", parent_y := i.parent_y]

    satellite_nodes[node_type == "variance", y := parent_y - 0.5]
    satellite_nodes[node_type == "intercept", y := parent_y + 0.5]

    nodes[satellite_nodes, on = "id", y := i.y]
  }

  return(nodes[, .(id, x, y)])
}

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

.calculate_xy_layout <- function(nodes, edges, flow_direction = "LR") {
  # --- 1. Initial Setup ---
  ranks <- .analyze_layout_structure(nodes, edges)

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
  main_nodes <- layout_dt[node_type == "manifest" | node_type == "latent"]
  setorder(main_nodes, rank, barycenter)

  # Assign evenly spaced positions to the main nodes (clusters)
  main_nodes[, (secondary_axis) := seq(-.N/2 + 0.5, .N/2 - 0.5, by = 1) * 2, by = rank]

  # Merge the calculated secondary positions back into the main layout table
  layout_dt[main_nodes, on = "id", (secondary_axis) := mget(paste0("i.", secondary_axis))]

  # Fill down the secondary coordinate for satellite nodes from their main node
  layout_dt[, (secondary_axis) := zoo::na.locf(.SD[[1]], na.rm = FALSE), by = node_unit, .SDcols = secondary_axis]

  # --- 4. Satellite Node Placement ---
  # Apply a fixed offset to satellite nodes along the secondary axis
  offset <- 0.5
  layout_dt[node_type == "variance", (secondary_axis) := .SD[[1]] + offset, .SDcols = secondary_axis]
  layout_dt[node_type == "intercept", (secondary_axis) := .SD[[1]] - offset, .SDcols = secondary_axis]

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

  # Pass the rankdir from the recipe to the new flow_direction argument
  layout <- .calculate_xy_layout(
    nodes = analyzed_model$nodes,
    edges = analyzed_model$edges,
    flow_direction = config$recipe$rankdir
  )

  .new_lavaan_layout(
    config = config,
    layout = layout
  )
}
