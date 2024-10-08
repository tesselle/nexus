## Data from Aitchison 1986
data("hongite")

## Coerce to compositional data
coda <- as_composition(hongite)

## Mean
mean(coda)

## Quantile
quantile(coda)
