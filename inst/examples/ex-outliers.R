## Coerce to chemical data
data("hongite")
coda <- as_composition(hongite)

## Detect outliers
out <- outliers(coda)

## Quantile-Quantile plot
qqplot(out)

## Plot
plot(out)
