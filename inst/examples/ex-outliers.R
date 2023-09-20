## Data from Day et al. 2011
data("kommos", package = "folio") # Coerce to compositional data
kommos <- remove_NA(kommos, margin = 1) # Remove cases with missing values
coda <- as_composition(kommos, groups = 1) # Use ceramic types for grouping

## Detect outliers
out <- outliers(coda, groups = NULL, robust = FALSE)

plot(out) # Plot
plot(out, qq = TRUE) # Quantile-Quantile plot

## Detect outliers by group
out <- outliers(coda[, 1:15, drop = FALSE])

plot(out, ncol = 2) # Plot
plot(out, qq = TRUE, ncol = 4) # Quantile-Quantile plot
