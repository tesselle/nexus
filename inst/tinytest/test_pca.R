Sys.setenv(LANGUAGE = "en") # Force locale

data("hongite")
coda <- as_composition(hongite)

expect_message(pca(coda), "PCA of centered log-ratio")
