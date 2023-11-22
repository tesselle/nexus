data("hongite")
coda <- as_composition(hongite)

# Mean =========================================================================
expect_equal_to_reference(mean(coda), file = "_snaps/mean.rds")

# Scale ========================================================================
z <- scale(coda, center = TRUE, scale = TRUE)
expect_equal(mean(z), c(A = 0.2, B = 0.2, C = 0.2, D = 0.2, E = 0.2))
expect_equal_to_reference(z, file = "_snaps/scale.rds")

# Margin =======================================================================
expect_equal_to_reference(margin(coda, parts = c("B", "D")), file = "_snaps/margin.rds")

# Metric variance ==============================================================
expect_equal(round(metric_var(coda), 5), 1.69132)

# Metric standard deviation ====================================================
expect_equal(round(metric_sd(coda), 5), 0.65025)

# Variation =====================================================================
expect_equal_to_reference(variation(coda), file = "_snaps/variation.rds")

# Covariance ===================================================================
expect_equal_to_reference(covariance(coda, center = FALSE), file = "_snaps/covariance_sigma.rds")
expect_equal_to_reference(covariance(coda, center = TRUE), file = "_snaps/covariance_tau.rds")

# Variation array ==============================================================
# expect_equal_to_reference(variation_array(coda), file = "_snaps/variation_array.rds")

# Aitchison distance ===========================================================
expect_equal_to_reference(
  dist(coda, method = "euclidean", diag = TRUE, upper = TRUE),
  file = "_snaps/dist_euclidean.rds"
)
