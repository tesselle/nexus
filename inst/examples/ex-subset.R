## Data from Aitchison 1986
data("hongite")

## Coerce to compositional data
coda <- as_composition(hongite)
head(coda)

## Subset
coda[[1, 1]] # Get the first value
coda[1] # Get the first value
coda[, ] # Get all values
coda[1, ] # Get the first row

## Subcomposition
subcoda <- coda[, 1:3] # Get the first three column
head(subcoda)
