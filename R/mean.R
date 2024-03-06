# STATISTICS: MEAN
#' @include AllGenerics.R
NULL

# Mean =========================================================================
#' @export
#' @method mean CompositionMatrix
mean.CompositionMatrix <- function(x, ..., na.rm = FALSE) {
  m <- apply(X = x, MARGIN = 2, FUN = gmean, na.rm = na.rm, simplify = TRUE)
  closure(m, na.rm = na.rm)
}

#' @export
#' @rdname mean
#' @aliases mean,CompositionMatrix-method
setMethod("mean", "CompositionMatrix", mean.CompositionMatrix)
