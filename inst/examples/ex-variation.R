## Data from Aitchison 1986
data("hongite")

## Coerce to compositional data
coda <- as_composition(hongite)

## Variation matrix
## (Aitchison 1986, definition 4.4)
(varia <- variation(coda))

## Cluster dendrogram
d <- as.dist(varia)
h <- hclust(d, method = "ward.D2")
plot(h)

## Heatmap
stats::heatmap(
  varia,
  distfun = stats::as.dist,
  hclustfun = function(x) stats::hclust(x, method = "ward.D2"),
  symm = TRUE,
  scale = "none"
)
