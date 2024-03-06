data("slides")
coda <- as_composition(slides, sample = 2, group = 1)

## Compositional mean by sample
by <- c("A", "A", "C", "D", "B", "E", "C", "B", "E", "D", "C", "E",
        "B", "E", "C", "D", "B", "C", "A", "B", "A", "C", "B", "A", "E")
flat <- flatten(coda, by = by)
expect_equal_to_reference(as_features(flat), file = "_snaps/flatten.rds")
