## Coerce to compositional data
data("hongite")
coda <- as_composition(hongite)

## Centered log-ratio
clr <- transform_clr(coda)

## PCA
X <- pca(clr)

## Plot
plot_individuals(X)
