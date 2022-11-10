## Coerce to compositonal data
data("hongite")
coda <- as_composition(hongite)

## Pairwise log-ratio
lr <- transform_lr(coda)
lr_graph <- graph(lr)
plot(lr_graph)

## Additive log-ratio
alr <- transform_alr(coda)
alr_graph <- graph(alr)
plot(alr_graph)

## Isometric log-ratio
ilr <- transform_ilr(coda)
ilr_graph <- graph(ilr)
plot(ilr_graph)

plr <- transform_plr(coda)
plr_graph <- graph(plr)
plot(plr_graph)
