## Data from Day et al. 2011
data("kommos", package = "folio") # Coerce to compositional data
kommos <- remove_NA(kommos, margin = 1) # Remove cases with missing values
coda <- as_composition(kommos, parts = 3:17, groups = 1)

## Detect outliers
out <- detect_outlier(coda)

plot(out, type = "dotchart")
plot(out, type = "distance")

## Detect outliers according to CJ
ref <- extract(coda, "CJ")
out <- detect_outlier(coda, reference = ref, method = "mcd")
plot(out, type = "dotchart")
