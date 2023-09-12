## Coerce to compositional data
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
