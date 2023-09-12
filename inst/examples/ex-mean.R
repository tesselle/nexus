## Coerce to compositional data
data("hongite")
coda <- as_composition(hongite)

## Mean
mean(coda)

## Metric variance
mvar(coda)

## Metric standard deviation
msd(coda)
