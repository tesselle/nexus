test_that("Barplot", {
  skip_if_not_installed("vdiffr")
  data("hongite")
  coda <- as_composition(hongite)

  gg_barplot <- autoplot(coda, order = NULL)
  vdiffr::expect_doppelganger("barplot", gg_barplot)

  gg_barplot_order <- autoplot(coda, order = 2)
  vdiffr::expect_doppelganger("barplot_order", gg_barplot_order)

  set_groups(coda) <- rep(1:5, 5)
  for (i in c(TRUE, FALSE)) {
    gg_barplot_facet <- autoplot(coda, facet = i)
    vdiffr::expect_doppelganger(paste0("barplot_facet-", i), gg_barplot_facet)
  }
})
