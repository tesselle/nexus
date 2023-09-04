data("hongite")
coda <- as_composition(hongite)

# Variance =====================================================================
expect_equal_to_reference(variance(coda), file = "_snaps/variance.rd")

# Covariance ===================================================================
expect_equal_to_reference(covariance(coda), file = "_snaps/covariance.rd")

# Variation ====================================================================
expect_equal_to_reference(variation(coda), file = "_snaps/variation.rd")

# Aitchison distance ===========================================================
expect_equal_to_reference(
  dist(coda, method = "euclidean", diag = TRUE, upper = TRUE),
  file = "_snaps/dist_euclidean.rd"
)
