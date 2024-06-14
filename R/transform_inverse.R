# DATA TRANSFORMATION: INVERSE
#' @include AllGenerics.R
NULL

# Backtransform ================================================================
## CLR -------------------------------------------------------------------------
#' @export
#' @rdname transform_inverse
#' @aliases transform_inverse,CLR,missing-method
setMethod(
  f = "transform_inverse",
  signature = c(object = "CLR", origin = "missing"),
  definition = function(object) {
    y <- methods::as(object, "matrix") # Drop slots
    y <- exp(y)
    y <- y / rowSums(y)

    dimnames(y) <- list(rownames(object), object@parts)
    .CompositionMatrix(
      y,
      totals = object@totals,
      samples = object@samples,
      groups = object@groups
    )
  }
)

## ALR -------------------------------------------------------------------------
#' @export
#' @rdname transform_inverse
#' @aliases transform_inverse,ALR,missing-method
setMethod(
  f = "transform_inverse",
  signature = c(object = "ALR", origin = "missing"),
  definition = function(object) {
    y <- exp(object)
    y <- y / (1 + rowSums(y))
    z <- 1 - rowSums(y)

    y <- cbind(y, z)
    dimnames(y) <- list(rownames(object), object@parts)
    y <- y[, object@order]

    .CompositionMatrix(
      y,
      totals = object@totals,
      samples = object@samples,
      groups = object@groups
    )
  }
)

## ILR -------------------------------------------------------------------------
#' @export
#' @rdname transform_inverse
#' @aliases transform_inverse,ILR,missing-method
setMethod(
  f = "transform_inverse",
  signature = c(object = "ILR", origin = "missing"),
  definition = function(object) {
    y <- tcrossprod(object@.Data, object@base)
    y <- exp(y)
    y <- y / rowSums(y)

    dimnames(y) <- list(rownames(object), object@parts)
    y <- y[, object@order]

    .CompositionMatrix(
      y,
      totals = object@totals,
      samples = object@samples,
      groups = object@groups
    )
  }
)

#' @export
#' @rdname transform_inverse
#' @aliases transform_inverse,matrix,ILR-method
setMethod(
  f = "transform_inverse",
  signature = c(object = "matrix", origin = "ILR"),
  definition = function(object, origin) {
    y <- tcrossprod(object, origin@base)
    y <- exp(y)
    y <- y / rowSums(y)

    dimnames(y) <- list(rownames(object), origin@parts)
    y <- y[, origin@order]

    y
  }
)
