## Coerce to chemical data
data("kommos", package = "folio")
coda <- as_composition(kommos[, -c(1, 2)])
coda <- remove_NA(coda, margin = 1)

## Detect outliers
out <- find_outliers(coda)
count_outliers(out)

## Plot
plot_outliers(out) +
  ggplot2::theme_bw() +
  khroma::scale_colour_contrast()

plot_outliers(out, coda, select = 1:6) +
  khroma::scale_colour_YlOrBr(range = c(0.3, 1))
