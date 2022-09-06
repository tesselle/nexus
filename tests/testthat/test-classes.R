test_that("CompositionMatrix", {
  # Empty instance
  expect_s4_class(.CompositionMatrix(), "CompositionMatrix")

  mtx <- CompositionMatrix(sample(1:100, 50, TRUE), ncol = 10)
  row_names <- paste0("row", 1:5)
  col_names <- paste0("col", 1:10)

  expect_equal(rownames(mtx), row_names)
  expect_equal(colnames(mtx), col_names)
  expect_equal(dimnames(mtx), list(row_names, col_names))

  # Try negative values
  expect_error(CompositionMatrix(data = c(-2, 3), nrow = 1),
               class = "arkhe_error_class")
})
