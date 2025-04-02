# Declare global variables to avoid CMD check notes
utils::globalVariables(c("table_x", "table_y", "join_type", "agg_x", "agg_y"))

#' Visualize Database Map
#'
#' Generate a visualization of the database map created by `create.DB.map`.
#' The visualization shows the relationships between tables, including linking variables,
#' join types, and potential aggregations.
#'
#' @param db_map A data.table object created by `create.DB.map`.
#' @param layout The layout algorithm to use for the graph. Default is "kk" (Kamada-Kawai).
#'               Other options include "fr" (Fruchterman-Reingold), "circle", "grid", etc.
#'
#' @return A graph visualization of the database map.
#'
#' @import data.table
#' @import igraph
#' @importFrom ggplot2 ggplot geom_point aes
#'
#' @examples
#' library(data.table)
#' table1 <- data.table(id = c(1, 2), value1 = c("A", "B"))
#' table2 <- data.table(id = c(2, 3), value2 = c("C", "D"))
#' tables <- list(table1 = table1, table2 = table2)
#' by <- list(list(by.x = "id", by.y = "id"))  # Corrected syntax
#' db_map <- create.DB.map(tables, by, join_types = "inner")
#' visualize.DB.map(db_map)
#'
#' @export
visualize.DB.map <- function(db_map, layout = "kk") {
  # Validate db_map input
  if (!inherits(db_map, "data.table")) {
    stop("db_map must be a data.table object created by create.DB.map.")
  }

  # Ensure required columns exist in db_map
  required_columns <- c("table_x", "table_y", "join_type", "agg_x", "agg_y")
  missing_cols <- setdiff(required_columns, colnames(db_map))
  if (length(missing_cols) > 0) {
    stop("Missing required columns in db_map: ", paste(missing_cols, collapse = ", "))
  }

  # Ensure db_map is not NULL or empty
  if (is.null(db_map) || nrow(db_map) == 0) {
    stop("db_map is empty or NULL.")
  }

  # Load necessary packages
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("The 'igraph' package is required for visualization. Please install it.")
  }

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("The 'ggplot2' package is required for visualization. Please install it.")
  }

  # Create a copy of db_map to avoid modifying a locked namespace
  db_map_copy <- data.table::copy(db_map)

  # Create edges for the graph
  edges <- db_map_copy[, list(
    from = table_x,
    to = table_y,
    join_type = join_type,
    label = paste0("Join: ", join_type, "\nAgg_x: ", agg_x, "\nAgg_y: ", agg_y)
  )]

  # Create nodes for the graph
  nodes <- unique(data.table(id = unique(c(db_map_copy$table_x, db_map_copy$table_y))))

  # Create a graph from the database map
  graph <- igraph::graph_from_data_frame(edges, directed = FALSE, vertices = nodes)

  # Set edge labels (join types and aggregations)
  igraph::E(graph)$label <- edges$label

  # Set node labels (table names)
  igraph::V(graph)$label <- igraph::V(graph)$name

  # Choose layout
  layout_fun <- switch(layout,
                       "kk" = igraph::layout_with_kk,
                       "fr" = igraph::layout_with_fr,
                       "circle" = igraph::layout_in_circle,
                       "grid" = igraph::layout_on_grid,
                       stop("Invalid layout. Choose from 'kk', 'fr', 'circle', or 'grid'.")
  )

  # Plot the graph
  plot(graph, layout = layout_fun(graph), vertex.label.cex = 1.2, edge.label.cex = 1.2)
}
