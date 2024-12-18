# Back transform to count ======================================================
data("hongite")
coda <- as_composition(hongite)
count <- as_amounts(coda)
expect_equal(as.data.frame(count), hongite)
