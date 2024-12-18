## Data from Aitchison 1986
data("slides")

## Coerce to compositional data
coda <- as_composition(slides, groups = 2)

## Grouping metadata
group_levels(coda)

group_names(coda)

group_indices(coda)

group_rows(coda)

group_length(coda)

group_size(coda)
