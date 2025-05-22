## Data from Day et al. 2011
data("kommos", package = "folio")
kommos <- remove_NA(kommos, margin = 1) # Remove cases with missing values
coda <- as_composition(kommos, parts = 3:22) # Coerce to compositional data

## Log ratio
clr <- transform_clr(coda)

## Boxplot
boxplot(clr)

## Use ceramic types for grouping
grp <- group(coda, by = kommos$type)
clr <- transform_clr(grp)
boxplot(clr)
