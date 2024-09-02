## Data from Day et al. 2011
data("kommos", package = "folio") # Coerce to compositional data
kommos <- remove_NA(kommos, margin = 1) # Remove cases with missing values
coda <- as_composition(kommos, groups = 1)

## Log ratio
clr <- transform_clr(coda)

## Density plot
plot(clr, by = NULL, flip = TRUE)

## Use ceramic types for grouping
plot(clr, flip = TRUE)
