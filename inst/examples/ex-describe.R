## Data from Aitchison 1986
data("slides")

## Coerce to compositional data
coda <- as_composition(slides, sample = 1, group = 2)

## Quick description
describe(coda)
