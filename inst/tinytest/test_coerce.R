# Data with groups =============================================================
data("slides")
coda <- as_composition(slides)
expect_equal_to_reference(augment(coda), file = "_snaps/augment_coda.rds")

clr <- transform_clr(coda, weights = FALSE)
expect_equal_to_reference(clr, file = "_snaps/augment_clr.rds")

# Back transform to count ======================================================
data("hongite")
coda <- as_composition(hongite)
count <- as_amounts(coda)
expect_equal(as.data.frame(count), hongite)
