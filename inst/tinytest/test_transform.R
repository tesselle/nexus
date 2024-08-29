data("hongite")
coda <- as_composition(hongite)

# LR ===========================================================================
lr <- transform_lr(coda)
expect_equal_to_reference(lr, file = "_snaps/transform_lr.rds")

# CLR ==========================================================================
clr <- transform_clr(coda, weights = FALSE)
expect_equal_to_reference(clr, file = "_snaps/transform_clr.rds")
x <- transform_inverse(clr)
expect_equal(coda, x)

wclr <- transform_clr(coda, weights = TRUE)
x <- transform_inverse(wclr)
expect_equal(coda, x)

alr <- transform_alr(coda)
y <- transform_clr(alr)
expect_equal(clr, y)

x <- transform_inverse(y)
expect_equal(coda, x)

# ilr <- transform_ilr(coda)
# z <- transform_clr(ilr)
# expect_identical(ilr, z)

# ALR ==========================================================================
alr <- transform_alr(coda, j = 2)
expect_equal_to_reference(alr, file = "_snaps/transform_alr.rds")

x <- transform_inverse(alr)
expect_equal(coda, x)

clr <- transform_clr(coda, weights = FALSE)
y <- transform_alr(clr, j = 2)
expect_equal(alr, y)

wclr <- transform_clr(coda, weights = TRUE)
y <- transform_alr(wclr, j = 2)
expect_equal(alr, y)

x <- transform_inverse(y)
expect_equal(coda, x)

# ilr <- transform_ilr(coda)
# z <- transform_alr(ilr)
# expect_identical(ilr, z)

# ILR ==========================================================================
base_classic <- nexus:::ilr_base(5, method = "basic")
expect_equal_to_reference(base_classic, file = "_snaps/ilr_base_classic.rds")

ilr <- transform_ilr(coda)
expect_equal_to_reference(ilr, file = "_snaps/transform_ilr.rds")

x <- transform_inverse(ilr)
expect_equal(coda, x)

clr <- transform_clr(coda)
y <- transform_ilr(clr)
expect_equal(ilr, y)

alr <- transform_alr(coda)
z <- transform_ilr(alr)
expect_equal(ilr, z)

# PLR ==========================================================================
plr <- transform_plr(coda)
expect_equal_to_reference(plr, file = "_snaps/transform_plr.rds")

plr2 <- transform_plr(coda, pivot = 2)
x <- transform_inverse(plr2)
expect_equal(coda, x)
