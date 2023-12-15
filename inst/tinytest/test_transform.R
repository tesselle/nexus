data("hongite")
coda <- as_composition(hongite)

# LR ===========================================================================
lr <- transform_lr(coda)
expect_equal_to_reference(lr, file = "_snaps/transform_lr.rds")

# CLR ==========================================================================
clr <- transform_clr(coda, weights = FALSE)
expect_equal_to_reference(clr, file = "_snaps/transform_clr.rds")

wclr <- transform_clr(coda, weights = TRUE)
x <- transform_inverse(wclr)
expect_equal(coda, x)

# alr <- transform_alr(coda)
# y <- transform_clr(alr)
# expect_identical(clr, y)

# ilr <- transform_ilr(coda)
# z <- transform_clr(ilr)
# expect_identical(ilr, z)

# ALR ==========================================================================
alr <- transform_alr(coda, j = 2)
expect_equal_to_reference(alr, file = "_snaps/transform_alr.rds")

x <- transform_inverse(alr)
expect_equal(coda, x)

# clr <- transform_clr(coda)
# y <- transform_alr(clr)
# expect_identical(alr, y)

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

# clr <- transform_clr(coda)
# y <- transform_ilr(clr)
# expect_identical(ilr, y)

# alr <- transform_alr(coda)
# z <- transform_ilr(alr)
# expect_identical(ilr, z)

# PLR ==========================================================================
plr <- transform_plr(coda)
expect_equal_to_reference(plr, file = "_snaps/transform_plr.rds")

plr2 <- transform_plr(coda, pivot = 2)
x <- transform_inverse(plr2)
expect_equal(coda, x)
