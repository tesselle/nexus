# Test against other packages ==================================================
if (at_home()) {
  data("hongite")
  coda <- as_composition(hongite)

  lr <- transform_lr(coda)
  clr <- transform_clr(coda)
  wclr <- transform_clr(coda, weights = TRUE)
  alr <- transform_alr(coda, j = 2)
  walr <- transform_alr(coda, j = 2, weights = TRUE)
  ilr <- transform_ilr(coda)
  plr <- transform_plr(coda)

  if (requireNamespace("compositions", quietly = TRUE)) {
    expect_equivalent(clr@.Data, as.matrix(compositions::clr(compositions::acomp(hongite))))
    expect_equivalent(alr@.Data, as.matrix(compositions::alr(compositions::acomp(hongite), ivar = 2)))
    expect_equivalent(ilr@.Data, as.matrix(compositions::ilr(compositions::acomp(hongite))))
    expect_equivalent(covariance(coda, center = TRUE), compositions::var(compositions::acomp(hongite), robust = FALSE))
    expect_equivalent(variation(coda), compositions::variation(compositions::acomp(hongite), robust = FALSE))
    expect_equivalent(dist(coda), compositions::dist(compositions::acomp(hongite)))
    expect_equivalent(variance_total(coda), compositions::mvar(compositions::acomp(compositions::acomp(hongite)), robust = FALSE))
    expect_equivalent(variance_total(coda, sd = TRUE), compositions::msd(compositions::acomp(compositions::acomp(hongite)), robust = FALSE))
    expect_equivalent(covariance(coda), compositions::cov(compositions::acomp(hongite), robust = FALSE))
  }
  if (requireNamespace("robCompositions", quietly = TRUE)) {
    expect_equivalent(clr@.Data, as.matrix(robCompositions::cenLR(hongite)$x.clr))
    expect_equivalent(alr@.Data, as.matrix(robCompositions::addLR(hongite, ivar = 2)$x.alr))
    expect_equivalent(plr@.Data, as.matrix(robCompositions::pivotCoord(hongite)))
    expect_equivalent(dist(coda), robCompositions::aDist(hongite))
  }
  if (requireNamespace("easyCODA", quietly = TRUE)) {
    expect_equivalent(lr@.Data, easyCODA::LR(hongite, weight = FALSE)$LR)
    expect_equivalent(clr@.Data, easyCODA::CLR(hongite, weight = FALSE)$LR)
    expect_equivalent(wclr@.Data, easyCODA::CLR(hongite, weight = TRUE)$LR)
    expect_equivalent(alr@.Data, easyCODA::ALR(hongite, denom = 2, weight = FALSE)$LR)
    expect_equivalent(walr@.Data, easyCODA::ALR(hongite, denom = 2, weight = TRUE)$LR)
    expect_equivalent(ilr@.Data[, 4] * -1, easyCODA::ILR(hongite, numer = 1:4, denom = 5, weight = FALSE)$LR)
    expect_equivalent(variance_total(clr), easyCODA::LR.VAR(clr, weight = clr@weights, vars = FALSE))
    expect_equivalent(variance(clr), easyCODA::LR.VAR(clr, weight = clr@weights, vars = TRUE)$LRvars)
  }
}
