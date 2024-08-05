## Data from Aitchison 1986
data("arctic")

## Coerce to compositional data
coda <- as_composition(arctic, parts = 1:3)

## Isometric log-ratio
ilr <- transform_ilr(coda)

## Add extra column
augment(ilr)
