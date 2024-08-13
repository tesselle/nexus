# Data with groups =============================================================
data("slides")
coda <- as_composition(slides, group = 1)
expect_equal_to_reference(coda, file = "_snaps/coerce.rds")

# Back transform to count ======================================================
data("hongite")
coda <- as_composition(hongite)
count <- as_amounts(coda)
expect_equal(as.data.frame(count), hongite)
