## Coerce to compositonal data
data("hongite")
coda <- as_composition(hongite)

## Distance
dist(coda)

## Mahalanobis distance
mahalanobis(coda)
