## Data from Aitchison 1986
data("slides")

## Coerce to compositional data
coda <- as_composition(slides, groups = 2)

## Quick description
describe(coda)
