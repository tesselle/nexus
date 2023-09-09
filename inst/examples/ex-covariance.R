## Coerce to compositional data
data("hongite")
coda <- as_composition(hongite)

## Variation matrix
## (Aitchison 1986, definition 4.4)
variance(coda)

## Log-ratio covariance matrix
## (Aitchison 1986, definition 4.5)
covariance(coda, center = FALSE)

## Centered log-ratio covariance matrix
## (Aitchison 1986, definition 4.6)
covariance(coda, center = TRUE)
