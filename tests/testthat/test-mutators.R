test_that("CompositionMatrix samples", {
  cts <- CompositionMatrix(sample(1:100, 75, TRUE), ncol = 5)

  expect_equal(get_samples(cts), paste0("row", 1:15))

  set_samples(cts) <- rep(c("A", "B", "C"), each = 5)
  expect_equal(get_samples(cts), rep(c("A", "B", "C"), each = 5))

  set_samples(cts) <- NULL
  expect_equal(get_samples(cts), paste0("row", 1:15))

  # Invalid values
  # Try wrong length
  expect_error(set_samples(cts) <- LETTERS, class = "arkhe_error_class")
})
test_that("CompositionMatrix groups", {
  cts <- CompositionMatrix(sample(1:100, 75, TRUE), ncol = 5)

  expect_equal(get_groups(cts), character(0))
  expect_false(has_groups(cts))

  set_groups(cts) <- rep(c("A", "B", "C"), each = 5)
  expect_true(has_groups(cts))
  expect_equal(get_groups(cts), rep(c("A", "B", "C"), each = 5))

  set_groups(cts) <- NULL
  expect_false(has_groups(cts))

  # Invalid values
  # Try wrong length
  expect_error(set_groups(cts) <- LETTERS, class = "arkhe_error_class")
})
test_that("CompositionMatrix totals", {
  mtx <- matrix(sample(1:100, 75, TRUE), ncol = 5)
  freq <- as_composition(mtx)

  expect_equal(get_totals(freq), rowSums(mtx), ignore_attr = TRUE)

  set_totals(freq) <- seq_len(15)
  expect_equal(get_totals(freq), seq_len(15))

  # Invalid values
  # Try negative values
  expect_error(set_totals(freq) <- -seq_len(10), class = "arkhe_error_class")
  # Try wrong length
  expect_error(set_totals(freq) <- 1, class = "arkhe_error_class")
})
