data("hongite")
coda <- as_composition(hongite)

# Detect outliers ==============================================================
out <- outliers(coda, robust = FALSE)
expect_equal_to_reference(as.data.frame(out), file = "_snaps/outliers.rds")

# Plot =========================================================================
if (at_home()) {
  using("tinysnapshot")
  options(tinysnapshot_device = "svglite")
  options(tinysnapshot_height = 7) # inches
  options(tinysnapshot_width = 7)
  options(tinysnapshot_tol = 200) # pixels
  options(tinysnapshot_os = "Linux")

  plot_outliers <- function() plot(out, qq = FALSE)
  expect_snapshot_plot(plot_outliers, "plot_outliers")

  plot_outliers_qqplot <- function() plot(out, qq = TRUE)
  expect_snapshot_plot(plot_outliers_qqplot, "plot_outliers_qqplot")
}
