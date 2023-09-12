data("hongite")
coda <- as_composition(hongite)

# Back transform to count ======================================================
count <- as_amounts(coda)
expect_equal(as.data.frame(count), hongite)
