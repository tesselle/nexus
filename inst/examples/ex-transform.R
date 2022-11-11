## Coerce to compositional data
data("hongite")
coda <- as_composition(hongite)

## Pairwise log-ratio
lr <- transform_lr(coda)

## Centered log-ratio
clr <- transform_clr(coda)

## Additive log-ratio
alr <- transform_alr(coda)

## Isometric log-ratio
ilr <- transform_ilr(coda)
plr <- transform_plr(coda)

## Inverse transformation
inv_clr <- transform_inverse(clr)
all.equal(coda, inv_clr)

inv_alr <- transform_inverse(alr)
all.equal(coda, inv_alr)

inv_ilr <- transform_inverse(ilr)
all.equal(coda, inv_ilr)

inv_plr <- transform_inverse(plr)
all.equal(coda, inv_plr)
