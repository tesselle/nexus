## Data from Day et al. 2011
data("kommos", package = "folio") # Coerce to compositional data
kommos <- remove_NA(kommos, margin = 1) # Remove cases with missing values
coda <- as_composition(kommos) # Use ceramic types for grouping

plot(coda[, 1:6, drop = FALSE], groups = "type")
plot(coda[, 1:6, drop = FALSE], groups = NULL)
