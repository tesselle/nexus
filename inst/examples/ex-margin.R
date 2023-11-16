## Coerce to compositional data
data("hongite")
coda <- as_composition(hongite)

## Marginal compositions
mar <- margin(coda, parts = c("B", "D"))
head(mar)
