library(testthat)
library(data.table)
library(DBmaps)

test_that("multi.merge works correctly", {
  table1 <- data.table(id = c(1, 2), value1 = c("A", "B"))
  table2 <- data.table(id = c(2, 3), value2 = c("C", "D"))
  table3 <- data.table(id = c(3, 4), value3 = c("E", "F"))
  tables <- list(table1, table2, table3)

  result <- multi.merge(tables, by = "id", types = c("outer", "left"))

  expect_equal(nrow(result), 3)  # ✅ Expect 3 rows (ids 1, 2, 3)
  expect_equal(result$id, c(1, 2, 3))  # ✅ Expect ids 1, 2, and 3
  expect_equal(result$value1, c("A", "B", NA))  # ✅ Expect value1 for id=1,2; NA for id=3
  expect_equal(result$value2, c(NA, "C", "D"))  # ✅ Expect value2 for id=2,3; NA for id=1
  expect_equal(result$value3, c(NA, NA, "E"))  # ✅ Expect value3 for id=3; NA for id=1,2
})


