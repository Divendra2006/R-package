library(testthat)
library(data.table)
library(DBmaps)

test_that("create.DB.map works correctly", {
  table1 <- data.table(id = c(1, 2), value1 = c("A", "B"))
  table2 <- data.table(id = c(2, 3), value2 = c("C", "D"))
  tables <- list(table1 = table1, table2 = table2)

  by <- list(c(by.x = "id", by.y = "id"))
  db_map <- create.DB.map(tables, by, join_types = "inner")

  expect_equal(nrow(db_map), 1)
  expect_equal(db_map$table_x, "table1")
  expect_equal(db_map$table_y, "table2")
  expect_true(db_map$can_join)
  expect_equal(db_map$by.x, "id")
  expect_equal(db_map$by.y, "id")
  expect_equal(db_map$join_type, "inner")
})

test_that("create.DB.map handles aggregations correctly", {
  table1 <- data.table(id = c(1, 2), value1 = c("A", "B"))
  table2 <- data.table(id = c(2, 3), value2 = c("C", "D"))
  tables <- list(table1 = table1, table2 = table2)

  by <- list(c(by.x = "id", by.y = "id"))
  aggregations <- list(list(x = "sum(value1)", y = "mean(value2)"))
  db_map <- create.DB.map(tables, by, join_types = "inner", aggregations = aggregations)

  expect_equal(db_map$agg_x, "sum(value1)")
  expect_equal(db_map$agg_y, "mean(value2)")
})
