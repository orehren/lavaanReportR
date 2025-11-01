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
#' @return A `lavaan_layout` object containing the original configuration and
#'   the new layout coordinates.
#' @keywords internal
#' @exportS3Method lavaanReportR::layout
#' @noRd
layout.lavaan_plot_config <- function(object, ...) {
  # 1. Call the core layout algorithm using the correct data paths
  nodes_layout_coords <- .calculate_xy_layout(
    nodes = object$analyzed_model$nodes,
    edges = object$analyzed_model$edges
  )

  # 2. Create and return the new lavaan_layout object using the constructor
  .new_lavaan_layout(config = object, layout = nodes_layout_coords)
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
  median_rank <- stats::median(unique(nodes$rank), na.rm = TRUE)
  nodes[, x := (rank - median_rank) * 3]

  # --- 3. Secondary Axis (Y-Coordinate) Calculation with Barycenter ---
  nodes[, y := as.numeric(NA)]
  nodes[, barycenter := as.numeric(NA)]
  for (r in sort(unique(nodes$rank))) {
    current_nodes_ids <- nodes[rank == r, id]
    parent_edges <- edges[to %in% current_nodes_ids]

    if (nrow(parent_edges) > 0) {
      parent_edges[nodes, on = .(from = id), parent_y := i.y]
      bary_lookup <- parent_edges[!is.na(parent_y), .(barycenter = mean(parent_y)), by = to]
      nodes[bary_lookup, on = .(id = to), barycenter := i.barycenter]
    }

    # Fallback for nodes without parents (e.g., in the first rank)
    nodes[rank == r & is.na(barycenter), barycenter := 0]

    # Sort nodes within the rank by barycenter and assign y-coordinates
    setorder(nodes, rank, barycenter)
    nodes[rank == r, y := (seq_len(.N) - (.N + 1) / 2) * 2]
  }

  # --- 4. Satellite Node Placement ---
  # This must happen *after* all main nodes have their final Y-positions.
  main_nodes_lookup <- nodes[node_type %in% c("latent", "manifest"), .(node_unit, parent_y = y)]
  nodes[main_nodes_lookup, on = .(node_unit), parent_y_for_sort := i.parent_y]
  setorder(nodes, rank, parent_y_for_sort, node_type)

  for (unit in unique(nodes[!is.na(node_unit), node_unit])) {
      main_node_y <- nodes[node_unit == unit & node_type %in% c("latent", "manifest"), y]
      if(length(main_node_y) > 0 && !is.na(main_node_y)){
        nodes[node_unit == unit & node_type == "variance", y := main_node_y - 0.5]
        nodes[node_unit == unit & node_type == "intercept", y := main_node_y + 0.5]
      }
  }

  # Clean up temporary columns before returning
  nodes[, c("barycenter", "parent_y_for_sort") := NULL]

  return(nodes[, .(id, x, y)])
}
