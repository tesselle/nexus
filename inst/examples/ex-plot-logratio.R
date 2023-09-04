## Coerce to compositional data
## Use ceramic types for grouping
data("kommos", package = "folio")
kommos <- remove_NA(kommos, margin = 1) # Remove cases with missing values
coda <- as_composition(kommos, groups = 1)

## Log ratio
clr <- transform_clr(coda)
plot(clr, group = NULL, flip = TRUE, border = "black", col = NA)
plot(clr, flip = TRUE)
