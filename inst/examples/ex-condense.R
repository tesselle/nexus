## Data from Aitchison 1986
data("slides")

## Coerce to a compositional matrix
coda <- as_composition(slides)

## Compositional mean by group
condense(coda, by = "slide")
