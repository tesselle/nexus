# DATA TRANSFORMATION: CENTERED LOG RATIO
#' @include AllGenerics.R
NULL

# CLR ==========================================================================
clr_base <- function(D, weights = rep(1 / D, D)) {
  diag(D) - matrix(data = weights, nrow = D, ncol = D)
}

#' @export
#' @rdname transform_clr
#' @aliases transform_clr,CompositionMatrix-method
setMethod(
  f = "transform_clr",
  signature = c(object = "CompositionMatrix"),
  definition = function(object, weights = FALSE) {
    D <- ncol(object)
    parts <- colnames(object)

    weights <- make_weights(object, weights = weights)
    base <- clr_base(D, weights = weights)
    clr <- log(object, base = exp(1)) %*% base
    dimnames(clr) <- dimnames(object)

    .CLR(
      clr,
      parts = parts,
      ratio = parts,
      order = seq_len(D),
      base = base,
      weights = weights,
      totals = total(object),
      groups = group(object)
    )
  }
)

#' @export
#' @rdname transform_clr
#' @aliases transform_clr,ALR-method
setMethod(
  f = "transform_clr",
  signature = c(object = "ALR"),
  definition = function(object) {
    D <- ncol(object) + 1
    w <- object@weights

    base <- clr_base(D, weights = w)
    clr <- object %*% base[-D, ]
    dimnames(clr) <- list(rownames(object), object@parts)

    .CLR(
      clr,
      parts = object@parts,
      ratio = object@parts,
      order = seq_len(D),
      base = base,
      weights = w,
      totals = total(object),
      groups = group(object)
    )
  }
)
