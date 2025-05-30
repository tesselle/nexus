Sys.setenv(LANGUAGE = "en") # Force locale

data("slides")
coda <- as_composition(slides)

## Ungrouped
flat <- condense(coda, by = slides$analyst)
expect_false(is_grouped(flat))
expect_identical(rownames(flat), paste0("A", 1:5))

## Compositional mean by group
coda <- group(coda, by = slides$analyst)
flat <- condense(coda)
expect_identical(group_names(flat), paste0("A", 1:5))

## Override groups
flat <- condense(coda, by = slides$slide)
expect_identical(rownames(flat), c("A", "B", "C", "D", "E"))
expect_identical(group_names(flat), rep(paste0("A", 1:5, collapse = ":"), 5))
expect_equal_to_reference(as.data.frame(flat), file = "_snaps/condense.rds")

## With zeros
X1 <- data.frame(
  Ca = c(7.72, 0, 3.11, 7.19, 7.41, 5, 0, 1, 4.51),
  Fe = c(6.12, 5.88, 5.12, 0, 6.02, 0, 0, 5.28, 5.72),
  Na = c(0.97, 1.59, 0, 0.86, 0.76, 0.51, 0.75, 0.52, 0.56)
)
Y1 <- as_composition(X1)

## With NA
X2 <- data.frame(
  Ca = c(7.72, NA, 3.11, 7.19, 7.41, 5, NA, 1, 4.51),
  Fe = c(6.12, 5.88, 5.12, NA, 6.02, NA, NA, 5.28, 5.72),
  Na = c(0.97, 1.59, NA, 0.86, 0.76, 0.51, 0.75, 0.52, 0.56)
)
Y2 <- as_composition(X2)

by <- c("A", "A", "A", "A", "B", "B", "B", "B", "C")
expect_equal(
  condense(Y1, by = by),
  condense(Y2, by = by, ignore_na = TRUE)
)
