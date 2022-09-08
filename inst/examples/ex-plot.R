## Coerce to compositonal data
data("hongite")
coda <- as_composition(hongite)

## Bar plot
plot(coda, order = 2)

## Use ceramic types for grouping
data("kommos", package = "folio")
kommos <- remove_NA(kommos, margin = 1) # Remove cases with missing values
coda <- as_composition(kommos, groups = 1)

plot(coda)
