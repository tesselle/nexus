# STATISTICS: QUANTILE
#' @include AllGenerics.R
NULL

# Quantile =====================================================================
#' @export
#' @method quantile CompositionMatrix
quantile.CompositionMatrix <- function(x, ..., probs = seq(0, 1, 0.25),
                                       na.rm = FALSE, names = TRUE) {
  apply(
    X = x,
    MARGIN = 2,
    FUN = stats::quantile,
    probs = probs,
    na.rm = na.rm,
    names = names,
    ...
  )
}

#' @export
#' @rdname quantile
#' @aliases quantile,CompositionMatrix-method
setMethod("quantile", "CompositionMatrix", quantile.CompositionMatrix)
