## Create a compositional matrix
A <- CompositionMatrix(data = sample(1:100, 100, TRUE), nrow = 20)

## Access
dim(A) # Get the matrix dimensions
row(A) # Get the row indexes
col(A, as.factor = TRUE) # Get the column indexes
nrow(A) # Get the number of rows
ncol(A) # Get the number of columns
dimnames(A) # Get the dimension names
rownames(A) <- LETTERS[1:20] # Set the row names
rownames(A) # Get the rownames
colnames(A) <- letters[21:25] # Set the column names
colnames(A) # Get the column names

## Subset
A[[1, 1]] # Get the first value
A[1] # Get the first value
A[, ] # Get all values
A[1, , drop = FALSE] # Get the first row
A[, 1:3] # Get the first three column
