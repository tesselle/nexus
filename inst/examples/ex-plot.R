## Data from Day et al. 2011
data("kommos", package = "folio")
kommos <- remove_NA(kommos, margin = 1) # Remove cases with missing values
coda <- as_composition(kommos, groups = 1) # Coerce to compositional data

## Log ratio
clr <- transform_clr(coda)

## Use ceramic types for grouping
plot(clr)
