Sys.setenv(LANGUAGE = "en") # Force locale

## Data from Aitchison 1986
data("slides")

# Describe =====================================================================
expect_stdout(describe(slides))
