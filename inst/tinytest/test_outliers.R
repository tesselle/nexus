data("arctic")
coda <- as_composition(arctic, parts = 1:3)

# Detect outliers ==============================================================
out <- detect_outlier(coda, robust = FALSE)
expect_equal_to_reference(out, file = "_snaps/detect_outlier.rds")

expect_equivalent(which(is_outlier(out, robust = FALSE)), c(7L, 12L))

# Plot =========================================================================
if (at_home()) {
  using("tinysnapshot")
  options(tinysnapshot_device = "svglite")
  options(tinysnapshot_height = 7) # inches
  options(tinysnapshot_width = 7)
  options(tinysnapshot_tol = 200) # pixels
  options(tinysnapshot_os = "Linux")

  plot_outliers <- function() plot(out, type = "dotchart", robust = FALSE)
  expect_snapshot_plot(plot_outliers, "plot_outliers_dotchart")
}
