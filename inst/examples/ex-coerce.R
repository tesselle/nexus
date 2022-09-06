## Create a count matrix
A1 <- matrix(data = as.numeric(sample(1:100, 100, TRUE)), nrow = 20)

## Coerce to compositions
B <- as_composition(A1)

## Row sums are internally stored before coercing to relative frequencies
## (use get_totals() to retrieve these values)
## This allows to restore the source data
A2 <- as_count(B)

## Coerce to an S3 data.frame
X <- data.frame(B)
head(X)
