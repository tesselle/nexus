## Data from Aitchison 1986
data("slides")

## Coerce to compositional data
coda <- as_composition(slides)

## Quick description
describe(coda)
