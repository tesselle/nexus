data("hongite")
coda <- as_composition(hongite)

# Aitchison distance ===========================================================
expect_equal_to_reference(
  dist(coda, method = "euclidean", diag = TRUE, upper = TRUE),
  file = "_snaps/dist_euclidean.rds"
)

# Mahalanobis distance =========================================================
expect_equal_to_reference(
  mahalanobis(coda, robust = FALSE),
  file = "_snaps/dist_mahalanobis.rds"
)
