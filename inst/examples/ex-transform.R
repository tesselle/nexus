## Coerce to compositonal data
data("hongite")
coda <- as_composition(hongite)

## Pairwise log-ratio
lr <- transform_lr(coda)

## Centered log-ratio
clr <- transform_clr(coda)

## Additive log-ratio
alr <- transform_alr(coda)

## Isometric log-ratio
ilr1 <- transform_ilr(coda)
ilr2 <- transform_plr(coda)
