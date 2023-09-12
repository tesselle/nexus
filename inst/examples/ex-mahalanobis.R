## Coerce to compositional data
data("hongite")
coda <- as_composition(hongite)

## Mahalanobis distance
mahalanobis(coda)
