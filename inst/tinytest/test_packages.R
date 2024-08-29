# Test against other packages ==================================================
if (at_home()) {
  data("hongite")
  coda <- as_composition(hongite)

  lr <- transform_lr(coda)
  clr <- transform_clr(coda, weights = FALSE)
  wclr <- transform_clr(coda, weights = TRUE)
  alr <- transform_alr(coda, j = 2)
  ilr <- transform_ilr(coda)
  plr <- transform_plr(coda)

  if (requireNamespace("compositions", quietly = TRUE)) {
    expect_equivalent(clr@.Data, as.matrix(compositions::clr(compositions::acomp(coda))))
    expect_equivalent(alr@.Data, as.matrix(compositions::alr(compositions::acomp(coda), ivar = 2)))
    expect_equivalent(ilr@.Data, as.matrix(compositions::ilr(compositions::acomp(coda))))
    expect_equivalent(covariance(coda, center = TRUE), compositions::var(compositions::acomp(coda), robust = FALSE))
    expect_equivalent(variation(coda), compositions::variation(compositions::acomp(coda), robust = FALSE))
    expect_equivalent(dist(coda), compositions::dist(compositions::acomp(coda)))
  }
  if (requireNamespace("robCompositions", quietly = TRUE)) {
    expect_equivalent(clr@.Data, as.matrix(robCompositions::cenLR(coda@.Data)$x.clr))
    expect_equivalent(alr@.Data, as.matrix(robCompositions::addLR(coda@.Data, ivar = 2)$x.alr))
    expect_equivalent(plr@.Data, as.matrix(robCompositions::pivotCoord(coda@.Data)))
    expect_equivalent(dist(coda), robCompositions::aDist(coda@.Data))
  }
  if (requireNamespace("easyCODA", quietly = TRUE)) {
    expect_equivalent(clr@.Data, easyCODA::CLR(coda@.Data, weight = FALSE)$LR)
    expect_equivalent(wclr@.Data, easyCODA::CLR(coda@.Data, weight = TRUE)$LR)
    expect_equivalent(alr@.Data, easyCODA::ALR(coda@.Data, denom = 2, weight = FALSE)$LR)
    expect_equivalent(lr@.Data, easyCODA::LR(coda@.Data, weight = FALSE)$LR)
  }
}
