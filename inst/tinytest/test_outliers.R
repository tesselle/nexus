Sys.setenv(LANGUAGE = "en") # Force locale

data("arctic")
coda <- as_composition(arctic, parts = 1:3)

# Detect outliers ==============================================================
out <- detect_outlier(coda, robust = FALSE)
expect_equal_to_reference(out, file = "_snaps/detect_outlier.rds")

expect_equivalent(which(is_outlier(out, robust = FALSE)), c(7L, 12L))

# Plot =========================================================================
if (at_home()) {
  using("tinysnapshot")
  source("helpers.R")

  plot_outliers <- function() plot(out, type = "dotchart", robust = FALSE)
  expect_snapshot_plot(plot_outliers, "plot_outliers_dotchart")
}
