# Data with groups =============================================================
data("slides")
coda <- as_composition(slides, sample = 2, group = 1)
expect_equal_to_reference(as_features(coda), file = "_snaps/features.rds")

# Back transform to count ======================================================
data("hongite")
coda <- as_composition(hongite)
count <- as_amounts(coda)
expect_equal(as.data.frame(count), hongite)
