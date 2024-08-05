## Data from Day et al. 2011
data("kommos", package = "folio") # Coerce to compositional data
kommos <- remove_NA(kommos, margin = 1) # Remove cases with missing values
coda <- as_composition(kommos, parts = 3:17)

## Detect outliers
out <- outliers(coda, groups = NULL, method = "mcd")

plot(out, type = "dotchart")
plot(out, type = "distance")
plot(out, type = "qqplot")

## Detect outliers by group
## (use ceramic types for grouping)
out <- outliers(coda, groups = "type", method = "mcd")

plot(out, type = "dotchart", select = 1, robust = FALSE)
plot(out, type = "dotchart", select = 2, robust = FALSE)
plot(out, type = "dotchart", select = 3, robust = FALSE)
plot(out, type = "dotchart", select = 4, robust = FALSE)
