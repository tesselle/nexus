## Coerce to compositonal data
data("hongite")
coda <- as_composition(hongite)

## Variance matrix
var(coda)

## Covariance matrix
cov(coda)

## Variation matrix
variation(coda)
