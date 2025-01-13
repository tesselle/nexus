## Data from Day et al. 2011
data("kommos", package = "folio") # Coerce to compositional data
kommos <- remove_NA(kommos, margin = 1) # Remove cases with missing values
coda <- as_composition(kommos, parts = 3:8, groups = 1)

## Use ceramic types for grouping
pairs(coda)

## Center and scale ternary plots
pairs(coda, by = NULL, center = TRUE, scale = TRUE)
