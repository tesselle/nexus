## Coerce to compositional data
data("hongite")
coda <- as_composition(hongite)

## Mean
mean(coda)

## Metric variance
metric_var(coda)

## Metric standard deviation
metric_sd(coda)
