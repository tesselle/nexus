## Data from Day et al. 2011
data("kommos", package = "folio") # Coerce to compositional data
kommos <- remove_NA(kommos, margin = 1) # Remove cases with missing values
coda <- as_composition(kommos, groups = 1) # Use ceramic types for grouping

## Detect outliers
out <- outliers(coda)

## Quantile-Quantile plot
plot(out, qq = TRUE)

## Plot
plot(out)
