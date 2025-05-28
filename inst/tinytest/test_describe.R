Sys.setenv(LANGUAGE = "en") # Force locale
using("tinysnapshot")

data("slides")

coda <- as_composition(slides)
expect_stdout(show(coda), "<CompositionMatrix: 25 x 7>")
expect_snapshot_print(round(describe(coda), 3), "describe_coda")

coda <- group(coda, by = slides$slide)
expect_stdout(show(coda), "<GroupedComposition: 25 x 7>")
expect_snapshot_print(round(describe(coda), 3), "describe_groups")
