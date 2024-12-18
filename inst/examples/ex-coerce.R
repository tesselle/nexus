## Create a count matrix
A1 <- matrix(data = sample(1:100, 100, TRUE), nrow = 20)

## Coerce to compositions
B <- as_composition(A1)

## Row sums are internally stored before coercing to relative frequencies
totals(B)

## This allows to restore the source data
A2 <- as_amounts(B)

## Coerce to a data.frame
X <- as.data.frame(B)
head(X)
