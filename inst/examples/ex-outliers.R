## Coerce to chemical data
data("hongite")
coda <- as_composition(hongite)

## Detect outliers
out <- outliers(coda)

## Plot
plot(out, qq = TRUE)
plot(out, qq = FALSE)
