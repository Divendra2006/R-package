#' Multi-Merge Tables
#'
#' Sequentially merge multiple tables based on a common key and join types.
#'
#' @param tables A list of data.tables to be merged.
#' @param by A character vector of column names to merge by.
#' @param types Type of join(s) to perform: "inner", "outer", "left", or "right".
#'              If a single value is provided, it will be used for all merges.
#'              If a vector is provided, it must have length equal to the number of tables minus one.
#'
#' @return A merged data.table.
#'
#' @examples
#' library(data.table)
#' table1 <- data.table(id = c(1, 2), value1 = c("A", "B"))
#' table2 <- data.table(id = c(2, 3), value2 = c("C", "D"))
#' table3 <- data.table(id = c(3, 4), value3 = c("E", "F"))
#' tables <- list(table1, table2, table3)
#' multi.merge(tables, by = "id", types = c("inner", "left"))
#'
#' @export
multi.merge <- function(tables, by, types = "inner") {
  if (!is.list(tables)) stop("tables must be a list of data.tables")
  if (length(tables) < 2) stop("At least two tables are required for merging.")

  if (length(types) == 1) {
    types <- rep(types, length(tables) - 1)
  } else if (length(types) != length(tables) - 1) {
    stop("Length of 'types' must be 1 or equal to number of tables minus 1.")
  }

  result <- tables[[1]]
  print("Initial Table:")
  print(result)

  for (i in 2:length(tables)) {
    type <- types[i - 1]
    print(paste("Merging with table", i, "using", type, "join"))
    print(tables[[i]])

    result <- switch(
      type,
      "inner" = merge(result, tables[[i]], by = by, all = FALSE),
      "left"  = merge(result, tables[[i]], by = by, all.x = TRUE, all.y = FALSE),
      "right" = merge(result, tables[[i]], by = by, all.x = FALSE, all.y = TRUE),
      "outer" = merge(result, tables[[i]], by = by, all = TRUE),
      stop("Invalid merge type. Use 'inner', 'left', 'right', or 'outer'.")
    )

    print("Result after this merge:")
    print(result)
  }

  return(result)
}
