data("hongite")
coda <- as_composition(hongite)

# Detect outliers ==============================================================
out <- outliers(coda, robust = FALSE)
expect_equal_to_reference(as_features(out), file = "_snaps/features_outliers.rds")

# Plot =========================================================================
if (at_home()) {
  using("tinysnapshot")
  options(tinysnapshot_device = "svglite")
  options(tinysnapshot_height = 7) # inches
  options(tinysnapshot_width = 7)
  options(tinysnapshot_tol = 200) # pixels
  options(tinysnapshot_os = "Linux")

  plot_outliers <- function() plot(out, type = "dotchart")
  expect_snapshot_plot(plot_outliers, "plot_outliers_dotchart")

  plot_outliers_qqplot <- function() plot(out, type = "qqplot")
  expect_snapshot_plot(plot_outliers_qqplot, "plot_outliers_qqplot")
}
