test_that("Variance", {
  data("hongite")
  coda <- as_composition(hongite)

  expect_snapshot(var(coda))
})
test_that("Covariance", {
  data("hongite")
  coda <- as_composition(hongite)

  expect_snapshot(cov(coda))
})
test_that("Variation", {
  data("hongite")
  coda <- as_composition(hongite)

  expect_snapshot(variation(coda))
})
test_that("Aitchison distance", {
  data("hongite")
  coda <- as_composition(hongite)

  expect_snapshot(dist(coda, method = "aitchison", diag = TRUE, upper = TRUE))
})
