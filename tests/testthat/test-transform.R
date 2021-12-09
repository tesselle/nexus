test_that("CLR", {
  data("hongite")
  coda <- as_composition(hongite)

  clr <- transform_clr(coda)
  expect_snapshot(clr)

  x <- transform_inverse(clr)
  expect_identical(round(coda@.Data, 3), round(x@.Data, 3))

  # alr <- transform_alr(coda)
  # y <- transform_clr(alr)
  # expect_identical(clr, y)

  # ilr <- transform_ilr(coda)
  # z <- transform_clr(ilr)
  # expect_identical(ilr, z)
})
test_that("ALR", {
  data("hongite")
  coda <- as_composition(hongite)

  alr <- transform_alr(coda, j = 2)
  expect_snapshot(alr)

  x <- transform_inverse(alr)
  expect_identical(round(coda@.Data, 3), round(x@.Data, 3))

  # clr <- transform_clr(coda)
  # y <- transform_alr(clr)
  # expect_identical(alr, y)

  # ilr <- transform_ilr(coda)
  # z <- transform_alr(ilr)
  # expect_identical(ilr, z)
})
test_that("ILR", {
  data("hongite")
  coda <- as_composition(hongite)

  ilr <- transform_ilr(coda)
  expect_snapshot(ilr)

  x <- transform_inverse(ilr)
  expect_identical(round(coda@.Data, 3), round(x@.Data, 3))

  # clr <- transform_clr(coda)
  # y <- transform_ilr(clr)
  # expect_identical(ilr, y)

  # alr <- transform_alr(coda)
  # z <- transform_ilr(alr)
  # expect_identical(ilr, z)
})
test_that("PLR", {
  data("hongite")
  coda <- as_composition(hongite)

  plr <- transform_pivot(coda)
  expect_snapshot(plr)

  # x <- transform_inverse(plr)
  # expect_identical(round(coda@.Data, 3), round(x@.Data, 3))
})
