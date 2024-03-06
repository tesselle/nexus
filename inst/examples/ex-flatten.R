## Data from Aitchison 1986
data("slides")

## Coerce to a compositional matrix
coda <- as_composition(slides, sample = 2, group = 1)

## Compositional mean by sample
flatten(coda, by = get_samples(coda))

## Compositional mean by group
flatten(coda, by = get_groups(coda))
