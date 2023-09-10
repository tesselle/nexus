data("hongite")
coda <- as_composition(hongite)

# Covariance ===================================================================
expect_equal_to_reference(covariance(coda, center = FALSE), file = "_snaps/covariance_sigma.rds")
expect_equal_to_reference(covariance(coda, center = TRUE), file = "_snaps/covariance_tau.rds")

# Variation =====================================================================
expect_equal_to_reference(variation(coda), file = "_snaps/variation.rds")

# Variation array ==============================================================
# expect_equal_to_reference(variation_array(coda), file = "_snaps/variation_array.rds")

# Aitchison distance ===========================================================
expect_equal_to_reference(
  dist(coda, method = "euclidean", diag = TRUE, upper = TRUE),
  file = "_snaps/dist_euclidean.rds"
)
