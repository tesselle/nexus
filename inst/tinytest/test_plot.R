if (at_home()) {
  using("tinysnapshot")
  options(tinysnapshot_device = "svglite")
  options(tinysnapshot_height = 7) # inches
  options(tinysnapshot_width = 7)
  options(tinysnapshot_tol = 200) # pixels
  options(tinysnapshot_os = "Linux")

  data("hongite")
  coda <- as_composition(hongite)

  # Plot =======================================================================
  plot_pairs <- function() pairs(coda)
  expect_snapshot_plot(plot_pairs, "plot_pairs")

  plot_pairs <- function() pairs(group(coda, by = rep(1:5, 5)))
  expect_snapshot_plot(plot_pairs, "plot_pairs_group")

  # Histogram ==================================================================
  plot_hist <- function() hist(coda, select = "B")
  expect_snapshot_plot(plot_hist, "plot_hist")

  plot_hist_count <- function() hist(coda, freq = TRUE, labels = TRUE)
  expect_snapshot_plot(plot_hist_count, "plot_hist_count")

  # Barplot ====================================================================
  plot_barplot <- function() barplot(coda, order_columns = FALSE, border = "black")
  expect_snapshot_plot(plot_barplot, "plot_barplot")

  plot_barplot_order <- function() barplot(coda, order_columns = TRUE, border = "black")
  expect_snapshot_plot(plot_barplot_order, "plot_barplot_order_columns")

  plot_barplot_order <- function() barplot(coda, order_rows = 2, border = "black")
  expect_snapshot_plot(plot_barplot_order, "plot_barplot_order_rows")

  plot_barplot_group <- function() barplot(group(coda, by = rep(1:5, 5)), order_columns = TRUE, border = "black")
  expect_snapshot_plot(plot_barplot_group, "plot_barplot_group")

  # Density ====================================================================
  ilr <- transform_ilr(coda)

  plot_ratio <- function() plot(ilr, pch = 16, factor = 0)
  expect_snapshot_plot(plot_ratio, "plot_ratio")

  gilr <- transform_ilr(group(coda, by = rep(1:5, 5)))

  plot_ratio <- function() plot(gilr, factor = 0)
  expect_snapshot_plot(plot_ratio, "plot_ratio_group")
}
