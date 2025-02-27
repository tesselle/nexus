data("slides")
coda <- as_composition(slides)

# Chemistry ====================================================================
expect_equal(nexus:::is_chemical(c("Al2O3", "Al2o3", "Al2O3xx")),
             c("Al2O3" = TRUE, "Al2o3" = FALSE, "Al2O3xx" = FALSE))
expect_equal(nexus:::label_chemical(c("Al2O3", "SiO2", "MnO", "K2O")),
             c("\"Al\"[2]*\"O\"[3]", "\"SiO\"[2]", "\"MnO\"", "\"K\"[2]*\"O\""))

expect_equal(is_oxide(c("Al2O3", "CO2", "Pb")),
             c("Al2O3" = TRUE, "CO2" = TRUE, "Pb" = FALSE))
expect_equal(is_oxide(c("CaCO3")), c("CaCO3" = FALSE))
expect_equal(is_oxide(c("XXo2")), c("XXo2" = FALSE))

major <- c(quartz = TRUE, microcline = TRUE, plagioclass = TRUE, biotite = TRUE,
           muscovite = TRUE, opaques = FALSE, nonopaques = FALSE)
expect_equal(is_element_major(coda), major)
expect_equal(is_element_minor(coda), !major)
