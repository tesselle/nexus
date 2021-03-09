## Coerce to chemical data
data("kommos", package = "folio")
coda <- as_composition(kommos[, -c(1, 2)])
coda <- remove_NA(coda, margin = 1)

## Centered log-ratio
clr <- transform_clr(coda)

## Additive log-ratio
alr <- transform_alr(coda, j = "Ca")

## Isometric log-ratio
ilr1 <- transform_ilr(coda)
ilr2 <- transform_pivot(coda)
