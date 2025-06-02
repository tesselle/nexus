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

# Mass to molar conversion
hydro_mass <- c(Na = 159, K = 6.318, Mg = 92.41, Ca = 279.1, Cl = 345.956092,
           SO4 = 730.60139, HCO3 = 349.5047) # mass
molar_weights <- c(22.99, 39.098, 34.305, 40.078, 35.453, 96.063, 61.017)
hydro_mol <- perturbation(as_composition(hydro_mass), 1 / molar_weights)

expect_equivalent(
  round(hydro_mol@.Data, 3),
  matrix(c(0.174, 0.004, 0.068, 0.175, 0.245, 0.191, 0.144), nrow = 1)
)

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
