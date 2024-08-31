## Data from Aitchison 1986
data("slides")

## Coerce to a compositional matrix
coda <- as_composition(slides)

## Compositional mean by slide
aggregate(coda, by = slides$slide, FUN = mean)

## Metric variance by slide
aggregate(coda, by = slides$slide, FUN = variance_total)
