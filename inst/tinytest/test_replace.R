Sys.setenv(LANGUAGE = "en") # Force locale

# Replace zeros ================================================================
X1 <- data.frame(
  Ca = c(7.72, 0, 3.11, 7.19, 7.41, 5, 0, 1, 4.51),
  Fe = c(6.12, 5.88, 5.12, 0, 6.02, 0, 0, 5.28, 5.72),
  Na = c(0.97, 1.59, 0, 0.86, 0.76, 0.51, 0.75, 0.52, 0.56)
)
Y1 <- as_composition(X1)

expect_error(transform_lr(Y1), "must not contain infinite values")
expect_error(transform_alr(Y1), "must not contain infinite values")
expect_error(transform_clr(Y1), "must not contain infinite values")
expect_error(transform_ilr(Y1), "must not contain infinite values")
expect_error(transform_plr(Y1), "must not contain infinite values")

## Remove zeros
expect_inherits(arkhe::remove_zero(Y1, margin = 1), "CompositionMatrix")
expect_inherits(arkhe::remove_zero(Y1, margin = 2), "CompositionMatrix")

## Multiplicative replacement
Z <- replace_zero(Y1, value = c(0.02, 0.1, 0.01), delta = 2/3)
expect_equal_to_reference(Z, file = "_snaps/zero_multiplicative.rds")

# Replace missing ==============================================================
X2 <- data.frame(
  Ca = c(7.72, NA, 3.11, 7.19, 7.41, 5, NA, 1, 4.51),
  Fe = c(6.12, 5.88, 5.12, NA, 6.02, NA, NA, 5.28, 5.72),
  Na = c(0.97, 1.59, NA, 0.86, 0.76, 0.51, 0.75, 0.52, 0.56)
)
Y2 <- as_composition(X2)

expect_error(transform_lr(Y2), "must not contain missing values")
expect_error(transform_alr(Y2), "must not contain missing values")
expect_error(transform_clr(Y2), "must not contain missing values")
expect_error(transform_ilr(Y2), "must not contain missing values")
expect_error(transform_plr(Y2), "must not contain missing values")

## Remove zeros
expect_inherits(arkhe::remove_NA(Y2, margin = 1), "CompositionMatrix")
expect_inherits(arkhe::remove_NA(Y2, margin = 2), "CompositionMatrix")

## Multiplicative replacement
Z <- replace_NA(Y2, value = 0)
expect_equal(Z, Y1)

Z <- replace_NA(Y2, value = 0.02)
expect_equal_to_reference(Z, file = "_snaps/missing_multiplicative.rds")
