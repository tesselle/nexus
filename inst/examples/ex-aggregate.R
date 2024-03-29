## Data from Aitchison 1986
data("slides")

## Coerce to a compositional matrix
coda <- as_composition(slides, sample = 2, group = 1)

## Compositional mean by sample
aggregate(coda, by = get_samples(coda), FUN = mean)

## Compositional mean by group
aggregate(coda, by = get_groups(coda), FUN = mean)

## Metric variance by group
aggregate(coda, by = get_groups(coda), FUN = metric_var)
