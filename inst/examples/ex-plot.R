## Data from Day et al. 2011
data("kommos", package = "folio") # Coerce to compositional data
kommos <- remove_NA(kommos, margin = 1) # Remove cases with missing values
coda <- as_composition(kommos, parts = 3:8)

## Use ceramic types for grouping
plot(coda, groups = "type")

## Center and scale ternary plots
plot(coda, groups = NULL, center = TRUE, scale = TRUE)
