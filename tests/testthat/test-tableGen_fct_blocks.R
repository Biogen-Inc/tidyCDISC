context("table generator ui")

block_data <- list(
  ADSL = data.frame(
    col_names = c("A", "B"),
    code = c("a", "b")
  ),
  ADAE = data.frame(
    col_names = c("C", "D"),
    code = c("c", "d")
  )
)

test_that("list element for each column name", {
  expect_equal(length(rowBlock(block_data[[1]], "test_class")), 2)
})

test_that("list elements contained in dataframe area", {
  expect_equal(length(rowPallete(block_data)), 2)
})

test_that("all blocks combine into single area", {
  expect_equal(length(rowArea(col = 1, block_data)[[1]]), 1)
})


test_that("drop area", {
  test_drop <- dropArea(col = 1, styles = "", "name", "outer_id", "inner_id", "drop_class")
  
  expect_equal(test_drop$children[[1]]$children[[1]], "name")
  expect_equal(test_drop$attribs$id, "outer_id")
  expect_equal(test_drop$children[[2]]$attribs$id, "inner_id")
  expect_equal(test_drop$children[[2]]$attribs$class, "drop_class")
})
