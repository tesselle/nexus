## Data from Aitchison 1986
data("slides")
head(slides)

## Coerce to compositional data
coda <- as_composition(slides, groups = 2)

get_groups(coda)
