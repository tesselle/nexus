## Data from Aitchison 1986
data("hongite")

## Coerce to compositional data
coda <- as_composition(hongite)

## Marginal compositions
mar <- margin(coda, parts = c("B", "D"))
head(mar)
