#' Create Database Map
#'
#' Construct a map of relationships between tables in a database. The map specifies
#' which tables can be joined, the linking variables, the type of join, and optional
#' aggregations within each table.
#'
#' @param tables A named list of data.tables, where the names are the table names.
#' @param by A list of character vectors specifying the linking variables for each pair of tables.
#'            Each element of the list should be a named vector with two elements:
#'            `by.x` (linking variable in the first table) and `by.y` (linking variable in the second table).
#' @param join_types A character vector specifying the type of join for each pair of tables.
#'                   Possible values: "inner", "outer", "left", "right".
#'                   If a single value is provided, it will be used for all pairs.
#' @param aggregations A list of aggregation functions to apply to each table.
#'                     Each element of the list should be a named list with two elements:
#'                     `x` (aggregation for the first table) and `y` (aggregation for the second table).
#'                     Default is `NULL` (no aggregation).
#'
#' @return A data.table with the following columns:
#'   - `table_x`: Name of the first table.
#'   - `table_y`: Name of the second table.
#'   - `can_join`: Logical indicating whether the tables can be joined.
#'   - `by.x`: Linking variable in the first table.
#'   - `by.y`: Linking variable in the second table.
#'   - `join_type`: Type of join.
#'   - `agg_x`: Aggregation for the first table (if specified).
#'   - `agg_y`: Aggregation for the second table (if specified).
#'
#' @examples
#' library(data.table)
#' table1 <- data.table(id = c(1, 2), value1 = c("A", "B"))
#' table2 <- data.table(id = c(2, 3), value2 = c("C", "D"))
#' tables <- list(table1 = table1, table2 = table2)
#' by <- list(c(by.x = "id", by.y = "id"))
#' db_map <- create.DB.map(tables, by, join_types = "inner")
#' print(db_map)
#'
#' @export
create.DB.map <- function(tables, by, join_types = "inner", aggregations = NULL) {
  # Input validation
  if (!is.list(tables) || is.null(names(tables))) {
    stop("'tables' must be a named list of data.tables.")
  }

  if (!is.list(by)) {
    stop("'by' must be a list of linking variable pairs.")
  }

  if (length(join_types) == 1) {
    join_types <- rep(join_types, length(by))
  } else if (length(join_types) != length(by)) {
    stop("Length of 'join_types' must be 1 or equal to the number of table pairs.")
  }

  if (!is.null(aggregations) && length(aggregations) != length(by)) {
    stop("Length of 'aggregations' must be equal to the number of table pairs.")
  }

  # Initialize the database map
  db_map <- data.table(
    table_x = character(),
    table_y = character(),
    can_join = logical(),
    by.x = character(),
    by.y = character(),
    join_type = character(),
    agg_x = character(),
    agg_y = character()
  )

  table_names <- names(tables)
  for (i in seq_along(by)) {
    pair <- by[[i]]
    if (!all(c("by.x", "by.y") %in% names(pair))) {
      stop("Each element of 'by' must contain 'by.x' and 'by.y'.")
    }

    table_x <- table_names[i]
    table_y <- table_names[i + 1]

    # Check if tables can be joined
    can_join <- all(pair["by.x"] %in% names(tables[[table_x]])) &&
      all(pair["by.y"] %in% names(tables[[table_y]]))

    # Add aggregations if specified
    agg_x <- if (!is.null(aggregations)) aggregations[[i]]$x else NA_character_
    agg_y <- if (!is.null(aggregations)) aggregations[[i]]$y else NA_character_

    # Add row to the database map
    db_map <- rbind(db_map, data.table(
      table_x = table_x,
      table_y = table_y,
      can_join = can_join,
      by.x = pair["by.x"],
      by.y = pair["by.y"],
      join_type = join_types[i],
      agg_x = agg_x,
      agg_y = agg_y
    ))
  }

  return(db_map)
}
