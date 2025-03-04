Sys.setenv(LANGUAGE = "en") # Force locale

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
