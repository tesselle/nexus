## Data from Aitchison 1986
data("hongite")

## Coerce to compositional data
coda <- as_composition(hongite)

## Bar plot
barplot(coda)

## Data from Day et al. 2011
data("kommos", package = "folio")
kommos <- remove_NA(kommos, margin = 1) # Remove cases with missing values
coda <- as_composition(kommos, groups = 1) # Coerce to compositional data

## Use ceramic types for grouping
barplot(coda, order_columns = TRUE)

## Display only minor elements
minor <- coda[, is_element_minor(coda)]
barplot(minor, order_columns = TRUE)
