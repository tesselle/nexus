# DATA TRANSFORMATION: CENTERED LOG RATIO
#' @include AllGenerics.R
NULL

# CLR ==========================================================================
#' @export
#' @rdname transform_clr
#' @aliases transform_clr,CompositionMatrix-method
setMethod(
  f = "transform_clr",
  signature = c(object = "CompositionMatrix"),
  definition = function(object, weights = FALSE) {
    J <- ncol(object)
    parts <- colnames(object)

    w <- if (any(weights)) colMeans(object) else rep(1 / J, J)
    if (is.numeric(weights)) {
      arkhe::assert_length(weights, J)
      arkhe::assert_positive(weights, strict = FALSE)
      w <- weights / sum(weights) # Sum up to 1
    }

    base <- diag(J) - matrix(data = w, nrow = J, ncol = J)
    clr <- log(object, base = exp(1)) %*% base
    dimnames(clr) <- dimnames(object)

    .CLR(
      clr,
      parts = parts,
      ratio = parts,
      order = seq_len(J),
      base = base,
      weights = w,
      totals = object@totals,
      samples = object@samples,
      groups = object@groups
    )
  }
)
