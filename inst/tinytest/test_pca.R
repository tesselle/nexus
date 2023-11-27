data("hongite")
coda <- as_composition(hongite)

expect_error(pca(coda), "Transform your data first")
