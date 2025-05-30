Sys.setenv(LANGUAGE = "en") # Force locale

x <- c(1, 2, 3)
y <- c(1, 2, 1)

# Closure ======================================================================
expect_identical(closure(x), x / sum(x))
expect_identical(closure(y), y / sum(y))

# Perturbation =================================================================
# c(0.125, 0.500, 0.375)
expect_identical(perturbation(x, y), closure(x * y))
expect_equivalent(perturbation(as_composition(x), y), as_composition(x * y))
expect_equivalent(perturbation(as_composition(x), as_composition(y)), as_composition(x * y))
expect_equivalent(as_composition(x) %perturbe% as_composition(y), as_composition(x * y))
expect_equivalent(as_composition(x) %perturbe% - as_composition(y), as_composition(x / y))

# Powering =====================================================================
# c(1^2, 2^2, 1^2) / 6
expect_identical(powering(y, 2), closure(y ^ 2))
expect_equivalent(as_composition(y) %power% 2, as_composition(y^2))
expect_equivalent(2 %power% as_composition(y), as_composition(y^2))

# Scalar product ===============================================================
expect_equal(round(scalar(x, y), 5), 0.06647)
expect_equal(round(scalar(as_composition(x), as_composition(y)), 5), 0.06647)

# Aitchison Distance ===========================================================
data("hongite")
coda <- as_composition(hongite)
expect_equivalent(nexus:::aitchison(coda), dist(coda, method = "euclidean"))
