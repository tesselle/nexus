## Data from Aitchison 1986
data("slides")
head(slides)

## Coerce to compositional data
coda <- as_composition(slides)
head(as_features(coda))
