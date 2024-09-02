# CompositionMatrix groups =====================================================
data("hongite")
coda <- as_composition(hongite)

expect_equal(group(coda), rep(NA_character_, nrow(coda)))
expect_false(any_assigned(coda))

group(coda) <- rep(c("A", "B", "C", "D", NA), each = 5)
expect_equal(group(coda), rep(c("A", "B", "C", "D", NA), each = 5))
expect_true(any_assigned(coda))
expect_equal(is_assigned(coda), rep(c(TRUE, FALSE), c(20, 5)))

group(coda) <- NULL
expect_false(any_assigned(coda))

# Invalid values
# Try wrong length
expect_error(group(coda) <- LETTERS, class = "arkhe_error_class")

# CompositionMatrix totals =====================================================
mtx <- matrix(sample(1:100, 75, TRUE), ncol = 5)
coda <- as_composition(mtx)

expect_equal(totals(coda), rowSums(mtx), ignore_attr = TRUE)

totals(coda) <- seq_len(15)
expect_equal(totals(coda), seq_len(15))

# Invalid values
# Try negative values
expect_error(totals(coda) <- -seq_len(10), class = "arkhe_error_class")
# Try wrong length
expect_error(totals(coda) <- 1, class = "arkhe_error_class")
