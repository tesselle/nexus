data("hongite")
coda <- as_composition(hongite)

# CompositionMatrix identifiers ================================================
expect_equal(get_identifiers(coda), rownames(hongite))

set_identifiers(coda) <- rep("A", 25)
expect_equal(get_identifiers(coda), paste("A", seq_len(25), sep = "_"))

set_identifiers(coda) <- NULL
expect_equal(get_identifiers(coda), get_samples(coda))

# Invalid values
# Try wrong length
expect_error(set_identifiers(coda) <- LETTERS, class = "arkhe_error_class")

# CompositionMatrix samples ====================================================
set_samples(coda) <- rep(c("A", "B", "C", "D", "E"), each = 5)
expect_equal(get_samples(coda), rep(c("A", "B", "C", "D", "E"), each = 5))
expect_true(any_replicated(coda))
expect_true(all(is_replicated(coda)))

set_samples(coda) <- NULL
expect_equal(get_samples(coda), paste0("S", seq_len(25)))

# Invalid values
# Try wrong length
expect_error(set_samples(coda) <- LETTERS, class = "arkhe_error_class")

# CompositionMatrix groups =====================================================
expect_equal(get_groups(coda), rep(NA_character_, nrow(coda)))
expect_false(any_assigned(coda))

set_groups(coda) <- rep(c("A", "B", "C", "D", NA), each = 5)
expect_equal(get_groups(coda), rep(c("A", "B", "C", "D", NA), each = 5))
expect_true(any_assigned(coda))
expect_equal(is_assigned(coda), rep(c(TRUE, FALSE), c(20, 5)))

set_groups(coda) <- NULL
expect_false(any_assigned(coda))

# Invalid values
# Try wrong length
expect_error(set_groups(coda) <- LETTERS, class = "arkhe_error_class")

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
