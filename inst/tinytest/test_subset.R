# Extract with an index vector =================================================
mtx <- matrix(data = sample(2:10, 100, TRUE), ncol = 10)
cts <- as_composition(mtx)
mtx <- mtx / rowSums(mtx)

## x[]
expect_true(all(cts[] == mtx))
expect_inherits(cts[], "CompositionMatrix")
## x[, ]
expect_true(all(cts[, ] == mtx))
expect_inherits(cts[, ], "CompositionMatrix")
## x[, , ]
expect_error(cts[, , ])

## x[i=]
expect_equal(cts[i = 1:2], mtx[i = 1:2])
## x[i=, ]
expect_inherits(cts[1:2, ], "CompositionMatrix")
## x[i=, , ]
expect_error(cts[1:2, , ])

## x[i=, drop=]
expect_equal(cts[1:2, drop = FALSE], mtx[1:2, drop = FALSE])
## x[i=, , drop=]
expect_equivalent(cts[1, , drop = TRUE], mtx[1, ])
expect_null(dim(cts[1, , drop = TRUE]))
expect_equivalent(cts[1:2, , drop = TRUE], mtx[1:2, ])
expect_equal(dim(cts[1, , drop = FALSE]), c(1, 10))
expect_equal(dim(cts[1:2, , drop = FALSE]), c(2, 10))
## x[i=, , , drop=]
expect_error(cts[1:2, , , drop = FALSE])

# x[j=]
expect_equal(cts[j = 3:4], mtx[j = 3:4])
## x[, j=]
expect_inherits(cts[, 3:4], "CompositionMatrix")
## x[, j=, ]
expect_error(cts[, 3:4, ])

## x[j=, drop=]
expect_equal(cts[j = 3:4, drop = FALSE], mtx[j = 3:4, drop = FALSE])
#x[, j=, drop=]
expect_equivalent(cts[, 3:4, drop = TRUE], mtx[, 3:4])
expect_equal(dim(cts[, 3, drop = FALSE]), c(10, 1))
expect_equal(dim(cts[, 3:4, drop = FALSE]), c(10, 2))
## x[, j=, , drop=]
expect_error(cts[, 3:4, , drop = FALSE])

## x[i=, j=]
expect_inherits(cts[1:2, 3:4], "CompositionMatrix")
## x[i=, j=, ]
expect_error(cts[1:2, 3:4, ])

## x[i=, j=, drop=]
expect_equivalent(cts[1, 3:4, drop = TRUE], mtx[1, 3:4])
expect_null(dim(cts[1, 3:4, drop = TRUE]))
expect_equivalent(cts[1:2, 3:4, drop = TRUE], mtx[1:2, 3:4])
expect_equal(dim(cts[1:2, 3:4, drop = FALSE]), c(2, 2))
## x[i=, j=, , drop=]
expect_error(cts[1:2, 3:4, , drop = FALSE])

## [[i=]]
expect_error(cts[[]])
expect_error(cts[[1:2]])
expect_error(cts[[, ]])
expect_error(cts[[1, ]])
expect_error(cts[[1:2, ]])
expect_error(cts[[, 1]])
expect_error(cts[[, 1:2]])

# Extract with a matrix ========================================================
expect_equal(cts[lower.tri(cts)], mtx[lower.tri(mtx)])

id <- matrix(data = c(2, 4, 6, 8, 10, 1, 2, 3, 4, 5), ncol = 2)
expect_equal(cts[id], mtx[id])

# Replace ======================================================================
cts[1] <- 1L
expect_equivalent(cts[1], 1)
expect_inherits(cts, "CompositionMatrix")

cts[1, 5] <- 0L
expect_equivalent(cts[1, 5, drop = TRUE], 0)
expect_inherits(cts, "CompositionMatrix")

cts[1, ] <- 0L
expect_equivalent(cts[1, , drop = TRUE], rep(0, 10))
expect_inherits(cts, "CompositionMatrix")

cts[, 1] <- 1L
expect_equivalent(cts[, 1, drop = TRUE], rep(1, 10))
expect_inherits(cts, "CompositionMatrix")

cts[[1]] <- 0L
expect_equal(cts[[1]], 0)
expect_inherits(cts, "CompositionMatrix")

cts[[1, 5]] <- 999L
expect_equal(cts[[1, 5]], 999)
expect_inherits(cts, "CompositionMatrix")

cts[] <- rep(0L, 100)
expect_equal(sum(cts), 0)

# Subcomposition ===============================================================
mtx <- matrix(data = sample(2:10, 100, TRUE), ncol = 10)
cts <- as_composition(mtx)

sub <- cts[, 1:3, drop = FALSE]
expect_equivalent(as_amounts(sub), mtx[, 1:3])

# Subset extra slots ===========================================================
mtx <- matrix(data = sample(2:10, 100, TRUE), ncol = 5)
cts <- as_composition(mtx)

expect_identical(get_totals(cts[1:5, , drop = FALSE]), get_totals(cts)[1:5])

set_groups(cts) <- rep(c("A", "B"), each = 10)
set_samples(cts) <- rep(c("X", "Y"), times = 10)

tmp <- cts[1:10, , drop = FALSE]
expect_identical(get_groups(tmp), rep("A", 10))
expect_identical(get_samples(tmp), rep(c("X", "Y"), times = 5))
