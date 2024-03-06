## Data from Aitchison 1986
data("hongite")

## Coerce to compositional data
coda <- as_composition(hongite)

## Additive log-ratio
alr <- transform_alr(coda)

## Inverse transformation
inv_alr <- transform_inverse(alr)
all.equal(coda, inv_alr)
