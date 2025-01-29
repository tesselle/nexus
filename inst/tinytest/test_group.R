# Data with groups =============================================================
data("slides")
coda <- as_composition(slides, group = NULL)
expect_equal_to_reference(coda, file = "_snaps/coerce_nogroup.rds")
coda <- as_composition(slides, group = 1)
expect_equal_to_reference(coda, file = "_snaps/coerce_group.rds")

expect_message(group(coda, by = slides$analyst, add = TRUE, verbose = TRUE))

expect_identical(group_levels(coda), c("A1", "A2", "A3", "A4", "A5"))
expect_identical(group_names(coda), slides$analyst)
expect_identical(group_indices(coda), c(1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L,
                                        3L, 3L, 3L, 3L, 3L, 4L, 4L, 4L, 4L, 4L,
                                        5L, 5L, 5L, 5L, 5L))
expect_identical(group_rows(coda), list(A1 = 1:5, A2 = 6:10, A3 = 11:15,
                                        A4 = 16:20, A5 = 21:25))
expect_identical(group_length(coda), 5L)
expect_identical(group_size(coda), c(A1 = 5L, A2 = 5L, A3 = 5L,
                                     A4 = 5L, A5 = 5L))
expect_identical(is_assigned(coda), rep(TRUE, 25))
expect_true(any_assigned(coda))
expect_true(all_assigned(coda))
expect_false(any_assigned(group(coda, by = rep(NA, nrow(coda)))))

expect_length(group_split(coda), 5)
expect_inherits(group_split(coda)[[1]], "CompositionMatrix")
expect_inherits(group_subset(coda, which = "A1"), "CompositionMatrix")

# Invalid values
# Try wrong length
expect_error(group(coda, by = LETTERS), class = "error_bad_length")
