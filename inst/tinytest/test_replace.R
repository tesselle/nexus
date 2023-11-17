# Replace zeros ================================================================
X <- data.frame(
  Ca = c(7.72, 0, 3.11, 7.19, 7.41, 5, 0, 1, 4.51),
  Fe = c(6.12, 5.88, 5.12, 0, 6.02, 0, 0, 5.28, 5.72),
  Na = c(0.97, 1.59, 0, 0.86, 0.76, 0.51, 0.75, 0.52, 0.56)
)
Y <- as_composition(X)

## Multiplicative replacement
Z <- replace_zero(Y, value = c(0.02, 0.1, 0.01), delta = 2/3)
expect_equal_to_reference(Z, file = "_snaps/zero_multiplicative.rds")

# Replace missing ==============================================================
X <- data.frame(
  Ca = c(7.72, NA, 3.11, 7.19, 7.41, 5, NA, 1, 4.51),
  Fe = c(6.12, 5.88, 5.12, NA, 6.02, NA, NA, 5.28, 5.72),
  Na = c(0.97, 1.59, NA, 0.86, 0.76, 0.51, 0.75, 0.52, 0.56)
)
Y <- as_composition(X)

## Multiplicative replacement
Z <- replace_NA(Y, value = 0.02)
expect_equal_to_reference(Z, file = "_snaps/missing_multiplicative.rds")
