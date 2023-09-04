# HELPERS

`%||%` <- function(x, y) {
  if (!is.null(x) && length(x) != 0) x else y
}

missingORnull <- function(x) {
  missing(x) || is.null(x)
}
