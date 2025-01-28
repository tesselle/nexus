# STATISTICS: MEAN
#' @include AllGenerics.R
NULL

# Mean =========================================================================
#' @export
#' @method mean CompositionMatrix
mean.CompositionMatrix <- function(x, ..., ignore_na = FALSE, ignore_zero = TRUE) {
  m <- apply(
    X = x,
    MARGIN = 2,
    FUN = gmean,
    ignore_na = ignore_na,
    ignore_zero = ignore_zero,
    simplify = TRUE
  )
  m <- closure(m)
  names(m) <- colnames(x)
  m
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
#' @param ignore_na A [`logical`] scalar: should [missing values][NA] be
#'  stripped before the computation proceeds?
#' @param ignore_zero A [`logical`] scalar: should zeros be stripped before the
#'  computation proceeds?
#' @return A [`numeric`] vector.
#' @keywords internal
gmean <- function(x, trim = 0, ignore_na = FALSE, ignore_zero = TRUE) {
  if (ignore_na) x <- x[is.finite(x)]
  if (ignore_zero) x <- x[x > 0]
  exp(mean(log(unclass(x)), trim = trim))
}
