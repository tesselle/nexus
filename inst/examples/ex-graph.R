## Coerce to compositional data
data("hongite")
coda <- as_composition(hongite)

## Pairwise log-ratio
lr <- transform_lr(coda)
lr_graph <- as_graph(lr)
plot(lr_graph)

## Additive log-ratio
alr <- transform_alr(coda)
alr_graph <- as_graph(alr)
plot(alr_graph)

## Isometric log-ratio
ilr <- transform_ilr(coda)
ilr_graph <- as_graph(ilr)
plot(ilr_graph)

plr <- transform_plr(coda)
plr_graph <- as_graph(plr)
plot(plr_graph)
