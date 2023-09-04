## Coerce to chemical data
data("hongite")
coda <- as_composition(hongite)

## Detect outliers
out <- outliers(coda)

## Quantile-Quantile plot
plot(out, qq = TRUE)

## Plot
plot(out)
