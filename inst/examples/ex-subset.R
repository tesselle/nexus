## Coerce to compositional data
data("hongite")
coda <- as_composition(hongite)
head(coda)

## Subset
coda[[1, 1]] # Get the first value
coda[1] # Get the first value
coda[, ] # Get all values
coda[1, , drop = FALSE] # Get the first row

## Subcomposition
subcoda <- coda[, 1:3, drop = FALSE] # Get the first three column
head(subcoda)