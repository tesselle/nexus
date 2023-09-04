# test_that("multiplication works", {
#   skip_if_not_installed("vdiffr")
#   data("hongite")
#   coda <- as_composition(hongite)
#
#   ## Detect outliers
#   out <- outliers(coda)
#   expect_snapshot(out)
#
#   ## Plot
#   gg_qqplot <- autoplot(coda, qq = TRUE)
#   vdiffr::expect_doppelganger("outliers_qqplot", gg_qqplot)
#
#   gg_plot <- autoplot(coda, qq = FALSE)
#   vdiffr::expect_doppelganger("outliers_plot", gg_plot)
# })
