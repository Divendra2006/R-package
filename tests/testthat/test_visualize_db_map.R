library(testthat)
library(data.table)
library(DBmaps)

test_that("visualize.DB.map works correctly", {
  table1 <- data.table(id = c(1, 2), value1 = c("A", "B"))
  table2 <- data.table(id = c(2, 3), value2 = c("C", "D"))
  tables <- list(table1 = table1, table2 = table2)

  by <- list(c(by.x = "id", by.y = "id"))
  db_map <- create.DB.map(tables, by, join_types = "inner", aggregations = list(list(x = "sum(value1)", y = "mean(value2)")))

  # Test that the function runs without errors
  expect_silent(visualize.DB.map(db_map))

  # Test that the function handles invalid layouts
  expect_error(visualize.DB.map(db_map, layout = "invalid"), "Invalid layout. Choose from 'kk', 'fr', 'circle', or 'grid'.")
})
