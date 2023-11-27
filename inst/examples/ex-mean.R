## Data from Aitchison 1986
data("hongite")

## Coerce to compositional data
coda <- as_composition(hongite)

## Mean
mean(coda)

## Metric variance
metric_var(coda)

## Metric standard deviation
metric_sd(coda)
