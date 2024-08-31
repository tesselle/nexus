## Data from Aitchison 1986
data("hongite")

## Coerce to compositional data
coda <- as_composition(hongite)

## Total variance (1)
variance_total(coda)

## Metric standard deviation
variance_total(coda, sd = TRUE)

## CLR transformation
clr <- transform_clr(coda)

## Individual log-ratio variances
variance(clr)

## Total log-ratio variance (2)
variance_total(clr)

## Proportionality between (1) and (2)
## See Aitchison 1997
variance_total(coda) * (1 / ncol(coda)) * (1 - (1 / nrow(coda)))
