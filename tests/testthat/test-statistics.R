test_that("Variance", {
  data("hongite")
  coda <- as_composition(hongite)

  expect_snapshot(variance(coda))
})
test_that("Covariance", {
  data("hongite")
  coda <- as_composition(hongite)

  expect_snapshot(covariance(coda))
})
test_that("Variation", {
  data("hongite")
  coda <- as_composition(hongite)

  expect_snapshot(variation(coda))
})
test_that("Aitchison distance", {
  data("hongite")
  coda <- as_composition(hongite)

  expect_snapshot(dist(coda, method = "euclidean", diag = TRUE, upper = TRUE))
})
