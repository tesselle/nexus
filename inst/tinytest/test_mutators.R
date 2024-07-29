data("hongite")
coda <- as_composition(hongite)

# CompositionMatrix totals =====================================================
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
