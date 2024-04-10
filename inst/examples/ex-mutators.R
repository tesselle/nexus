## Data from Aitchison 1986
data("slides")
head(slides)

## Coerce to compositional data
coda <- as_composition(slides, samples = 2, groups = 1)
head(as_features(coda))

get_samples(coda)
get_groups(coda)
