## Coerce to compositional data
data("hongite")
coda <- as_composition(hongite)

## Bar plot
barplot(coda, order = 2)

## Data from Day et al. 2011
data("kommos", package = "folio") # Coerce to compositional data
kommos <- remove_NA(kommos, margin = 1) # Remove cases with missing values
coda <- as_composition(kommos, groups = 1) # Use ceramic types for grouping

barplot(coda, order = 1, main = "Kommos Ceramics")
barplot(coda, order = 1, horiz = FALSE, main = "Kommos Ceramics")