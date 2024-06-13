## Data from Aitchison 1986
data("hongite")

## Coerce to compositional data
coda <- as_composition(hongite)

## Boxplot plot
hist(coda)
hist(coda[, 1, drop = FALSE])

univariate_ilr(coda)
