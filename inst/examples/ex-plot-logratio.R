## Data from Day et al. 2011
data("kommos", package = "folio") # Coerce to compositional data
kommos <- remove_NA(kommos, margin = 1) # Remove cases with missing values
coda <- as_composition(kommos, groups = 1) # Use ceramic types for grouping

## Log ratio
clr <- transform_clr(coda)
plot(clr, groups = NULL, flip = TRUE, border = "black", col = NA)
plot(clr, flip = TRUE)
