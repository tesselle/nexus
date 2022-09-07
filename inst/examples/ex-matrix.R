## Coerce to compositonal data
data("hongite")
coda <- as_composition(hongite)

## codaccess
dim(coda) # Get the matrix dimensions
row(coda) # Get the row indexes
col(coda, as.factor = TRUE) # Get the column indexes
nrow(coda) # Get the number of rows
ncol(coda) # Get the number of columns
dimnames(coda) # Get the dimension names
rownames(coda) <- LETTERS[1:25] # Set the row names
rownames(coda) # Get the rownames
colnames(coda) <- letters[21:25] # Set the column names
colnames(coda) # Get the column names

## Subset
coda[[1, 1]] # Get the first value
coda[1] # Get the first value
coda[, ] # Get all values
coda[1, , drop = FALSE] # Get the first row
coda[, 1:3] # Get the first three column
