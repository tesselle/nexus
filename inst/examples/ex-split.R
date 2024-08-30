## Create a data.frame
X <- data.frame(
  samples = c("A", "A", "A", "B", "B", "B", "C", "C", "C"),
  groups = c("X", "X", "X", "X", NA, NA, "Y", "Y", "Y"),
  Ca = c(7.72, 7.32, 3.11, 7.19, 7.41, 5, 4.18, 1, 4.51),
  Fe = c(6.12, 5.88, 5.12, 6.18, 6.02, 7.14, 5.25, 5.28, 5.72),
  Na = c(0.97, 1.59, 1.25, 0.86, 0.76, 0.51, 0.75, 0.52, 0.56)
)

## Coerce to a compositional matrix
Y <- as_composition(X)

## Split by group
## /!\ Unassigned samples are discarded ! /!\
(s1 <- split(Y, f = X$groups))

## Split by group
## Keep unassigned samples, see help(factor)
(s2 <- split(Y, f = factor(X$groups, exclude = NULL)))

## Bind by rows
do.call(rbind, s2)
