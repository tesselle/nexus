# Aggregate ====================================================================
data("slides")
petro <- as_composition(slides)

expect_equal_to_reference(aggregate(petro, by = slides$analyst, FUN = mean),
                          file = "_snaps/aggregate.rds")

# Mean =========================================================================
expect_equal(nexus:::gmean(c(7.72, 0, 3.11, 7.19), zero.rm = FALSE), 0)
expect_equal(nexus:::gmean(c(7.72, NA, 3.11, 7.19), na.rm = FALSE), NA_real_)
expect_equal(nexus:::gmean(c(7.72, 0, NA, 7.19), zero.rm = FALSE, na.rm = FALSE), NA_real_)
expect_equal(
  nexus:::gmean(c(7.72, 0, 3.11, 7.19), zero.rm = TRUE),
  nexus:::gmean(c(7.72, NA, 3.11, 7.19), na.rm = TRUE)
)

data("hongite")
coda <- as_composition(hongite)

expect_equal_to_reference(mean(coda), file = "_snaps/mean.rds")

# Quantile =====================================================================
qt <- quantile(coda)
expect_equal_to_reference(qt, file = "_snaps/quantile.rds")

# Scale ========================================================================
z <- scale(coda, center = TRUE, scale = TRUE)
expect_equal(mean(z), c(A = 0.2, B = 0.2, C = 0.2, D = 0.2, E = 0.2))
expect_equal_to_reference(z, file = "_snaps/scale.rds")

# Margin =======================================================================
mar <- margin(coda, parts = c("B", "D"))
expect_equal_to_reference(mar, file = "_snaps/margin.rds")

# Metric variance ==============================================================
expect_equal(round(variance_total(coda), 5), 1.69132)

clr <- transform_clr(coda)
lr <- transform_lr(coda)

expect_equal(
  variance_total(clr),
  variance_total(coda) * (1 / ncol(coda)) * (1 - (1 / nrow(coda)))
)

expect_equal(variance_total(clr), variance_total(lr))
expect_equal(round(variance_total(clr), 5), 0.32473)

expect_equal(round(variance(clr), 5), c(A = 0.01237, B = 0.10617, C = 0.18821, D = 0.00911, E = 0.00887))

# Metric standard deviation ====================================================
expect_equal(round(variance_total(coda, sd = TRUE), 5), 0.65025)

# Variation =====================================================================
expect_equal_to_reference(variation(coda), file = "_snaps/variation.rds")
expect_equal_to_reference(pip(coda), file = "_snaps/pip.rds")

# Covariance ===================================================================
expect_equal_to_reference(covariance(coda, center = FALSE), file = "_snaps/covariance_sigma.rds")
expect_equal_to_reference(covariance(coda, center = TRUE), file = "_snaps/covariance_tau.rds")

# Variation array ==============================================================
# expect_equal_to_reference(variation_array(coda), file = "_snaps/variation_array.rds")

# Log-ratio variance ===========================================================
