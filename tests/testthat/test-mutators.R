test_that("CompositionMatrix samples", {
  data("hongite")
  coda <- as_composition(hongite)

  expect_equal(get_samples(coda), rownames(hongite))

  set_samples(coda) <- rep(c("A", "B", "C", "D", "E"), each = 5)
  expect_equal(get_samples(coda), rep(c("A", "B", "C", "D", "E"), each = 5))

  set_samples(coda) <- NULL
  expect_equal(get_samples(coda), rownames(hongite))

  # Invalid values
  # Try wrong length
  expect_error(set_samples(coda) <- LETTERS, class = "arkhe_error_class")
})
test_that("CompositionMatrix groups", {
  data("hongite")
  coda <- as_composition(hongite)

  expect_equal(get_groups(coda), character(0))
  expect_false(has_groups(coda))

  set_groups(coda) <- rep(c("A", "B", "C", "D", "E"), each = 5)
  expect_true(has_groups(coda))
  expect_equal(get_groups(coda), rep(c("A", "B", "C", "D", "E"), each = 5))

  set_groups(coda) <- NULL
  expect_false(has_groups(coda))

  # Invalid values
  # Try wrong length
  expect_error(set_groups(coda) <- LETTERS, class = "arkhe_error_class")
})
test_that("CompositionMatrix totals", {
  mtx <- matrix(sample(1:100, 75, TRUE), ncol = 5)
  coda <- as_composition(mtx)

  expect_equal(get_totals(coda), rowSums(mtx), ignore_attr = TRUE)

  set_totals(coda) <- seq_len(15)
  expect_equal(get_totals(coda), seq_len(15))

  # Invalid values
  # Try negative values
  expect_error(set_totals(coda) <- -seq_len(10), class = "arkhe_error_class")
  # Try wrong length
  expect_error(set_totals(coda) <- 1, class = "arkhe_error_class")
})
