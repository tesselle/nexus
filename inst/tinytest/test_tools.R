data("slides")
coda <- as_composition(slides)

# Chemistry ====================================================================
major <- c(quartz = TRUE, microcline = TRUE, plagioclass = TRUE, biotite = TRUE,
           muscovite = TRUE, opaques = FALSE, nonopaques = FALSE)
expect_equal(is_element_major(coda), major)
expect_equal(is_element_minor(coda), !major)

expect_equal(is_oxide(c("Al2O3", "CO2", "Pb")), c("Al2O3" = TRUE, "CO2" = TRUE, "Pb" = FALSE))
expect_equal(is_oxide(c("XXo2")), c("XXo2" = FALSE))
