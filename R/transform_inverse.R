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
    .CompositionMatrix(y, totals = totals(object))
  }
)

#' @export
#' @rdname transform_inverse
#' @aliases transform_inverse,GroupedCLR,missing-method
setMethod(
  f = "transform_inverse",
  signature = c(object = "GroupedCLR", origin = "missing"),
  definition = function(object) {
    z <- methods::callNextMethod()
    .GroupedComposition(z, group_indices = group_indices(object),
                        group_levels = group_levels(object),
                        group_ordered = is_ordered(object))
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

    .CompositionMatrix(y, totals = totals(object))
  }
)

#' @export
#' @rdname transform_inverse
#' @aliases transform_inverse,GroupedALR,missing-method
setMethod(
  f = "transform_inverse",
  signature = c(object = "GroupedALR", origin = "missing"),
  definition = function(object) {
    z <- methods::callNextMethod()
    .GroupedComposition(z, group_indices = group_indices(object),
                        group_levels = group_levels(object),
                        group_ordered = is_ordered(object))
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

    .CompositionMatrix(y, totals = totals(object))
  }
)

#' @export
#' @rdname transform_inverse
#' @aliases transform_inverse,GroupedILR,missing-method
setMethod(
  f = "transform_inverse",
  signature = c(object = "GroupedILR", origin = "missing"),
  definition = function(object) {
    z <- methods::callNextMethod()
    .GroupedComposition(z, group_indices = group_indices(object),
                        group_levels = group_levels(object),
                        group_ordered = is_ordered(object))
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
