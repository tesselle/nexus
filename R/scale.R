# SCALE
#' @include AllGenerics.R
NULL

# Scale ========================================================================
#' @export
#' @method scale CompositionMatrix
scale.CompositionMatrix <- function(x, center = TRUE, scale = TRUE) {
  if (isFALSE(center) & isFALSE(scale)) return(x)

  y <- x
  if (!isFALSE(center)) {
    if (isTRUE(center)) center <- mean(x)
    arkhe::assert_type(center, "numeric")
    arkhe::assert_length(center, NCOL(x))

    y <- perturbation(y, 1 / center)
  }

  if (!isFALSE(scale)) {
    if (isTRUE(scale)) scale <- sqrt(mean(diag(covariance(x, center = TRUE))))
    arkhe::assert_scalar(scale, "numeric")

    y <- powering(y, 1 / scale)
  }

  y
}

#' @export
#' @rdname scale
#' @aliases scale,CompositionMatrix-method
setMethod("scale", "CompositionMatrix", scale.CompositionMatrix)
