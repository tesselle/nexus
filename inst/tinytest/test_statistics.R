data("hongite")
coda <- as_composition(hongite)

# Variance =====================================================================
expect_equal_to_reference(variance(coda), file = "_snaps/variance.rds")

# Covariance ===================================================================
expect_equal_to_reference(covariance(coda), file = "_snaps/covariance.rds")

# Variation ====================================================================
expect_equal_to_reference(variation(coda), file = "_snaps/variation.rds")

# Aitchison distance ===========================================================
expect_equal_to_reference(
  dist(coda, method = "euclidean", diag = TRUE, upper = TRUE),
  file = "_snaps/dist_euclidean.rds"
)