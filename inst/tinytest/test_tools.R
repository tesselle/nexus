data("slides")
coda <- as_composition(slides)

# Chemistry ====================================================================
major <- c(quartz = TRUE, microcline = TRUE, plagioclass = TRUE, biotite = TRUE,
           muscovite = TRUE, opaques = FALSE, nonopaques = FALSE)
expect_equal(element_major(coda), major)
expect_equal(element_minor(coda), !major)
