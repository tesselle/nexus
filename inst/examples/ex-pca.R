## Data from Day et al. 2011
data("kommos", package = "folio") # Coerce to compositional data
kommos <- remove_NA(kommos, margin = 1) # Remove cases with missing values
coda <- as_composition(kommos, groups = 1) # Use ceramic types for grouping

## Centered log-ratio
clr <- transform_clr(coda)

## PCA
X <- pca(clr, scale = FALSE)

## Explore results
viz_individuals(X, highlight = get_groups(coda), pch = 16)
viz_variables(X)
