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
#' @return A `lavaan_layout` object, which is the original `lavaan_plot_config` object
#'   with the calculated layout data (`$nodes_layout_coords`) added.
#' @keywords internal
#' @noRd
layout.lavaan_plot_config <- function(object, ...) {
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
