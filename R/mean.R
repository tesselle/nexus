# STATISTICS: MEAN
#' @include AllGenerics.R
NULL

# Mean =========================================================================
#' @export
#' @method mean CompositionMatrix
mean.CompositionMatrix <- function(x, ..., na.rm = FALSE) {
  m <- apply(X = x, MARGIN = 2, FUN = gmean, na.rm = na.rm, simplify = TRUE)
  closure(m)
}

#' @export
#' @rdname mean
#' @aliases mean,CompositionMatrix-method
setMethod("mean", "CompositionMatrix", mean.CompositionMatrix)

#' Geometric Mean
#'
#' @param x A [`numeric`] vector.
#' @param trim A length-one [`numeric`] vector specifying the fraction (0 to 0.5)
#'  of observations to be trimmed from each end of `x` before the mean is
#'  computed.
#' @param na.rm A [`logical`] scalar: should `NA` values be stripped before the
#'  computation proceeds?
#' @return A [`numeric`] vector.
#' @keywords internal
#' @noRd
gmean <- function(x, trim = 0, na.rm = FALSE) {
  if (na.rm) x <- x[is.finite(x)]
  x <- x[x > 0]
  exp(mean(log(x), trim = trim))
}
