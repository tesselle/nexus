## Data from Day et al. 2011
data("kommos", package = "folio") # Coerce to compositional data
kommos <- remove_NA(kommos, margin = 1) # Remove cases with missing values
coda <- as_composition(kommos, groups = 1) # Use ceramic types for grouping

## Log-Ratio Analysis
X <- pca(coda)

## Biplot
biplot(X)

## Explore results
viz_individuals(X, extra_quali = group_names(coda))
viz_variables(X)
