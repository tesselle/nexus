# DATA TRANSFORMATION: ADDITIVE LOG RATIO
#' @include AllGenerics.R
NULL

# ALR ==========================================================================
alr_base <- function(D) {
  V <- diag(1, nrow = D, ncol = D - 1)
  V[D, ] <- -1

  V
}

#' @export
#' @rdname transform_alr
#' @aliases transform_alr,CompositionMatrix-method
setMethod(
  f = "transform_alr",
  signature = c(object = "CompositionMatrix"),
  definition = function(object, j = ncol(object), weights = FALSE) {
    D <- ncol(object)
    parts <- colnames(object)

    ## Reorder (move denominator)
    j <- if (is.character(j)) which(parts == j) else as.integer(j)
    ordering <- c(which(j != seq_len(D)), j)
    parts <- parts[ordering]
    z <- object[, ordering, drop = FALSE]

    ## Compute ratios
    base <- alr_base(D)
    alr <- log(z, base = exp(1)) %*% base
    rownames(alr) <- rownames(object)
    colnames(alr) <- paste(parts[-D], parts[D], sep = "_")

    ## Compute weights
    weights <- make_weights(object, weights = weights)

    .ALR(
      alr,
      parts = parts,
      ratio = paste(parts[-D], parts[D], sep = "/"),
      order = order(ordering),
      base = base,
      weights = weights,
      totals = totals(object)
    )
  }
)

#' @export
#' @rdname transform_alr
#' @aliases transform_alr,GroupedComposition-method
setMethod(
  f = "transform_alr",
  signature = c(object = "GroupedComposition"),
  definition = function(object, j = ncol(object), weights = FALSE) {
    z <- methods::callNextMethod()
    .GroupedALR(z, group_indices = group_indices(object),
                group_levels = group_levels(object))
  }
)

#' @export
#' @rdname transform_alr
#' @aliases transform_alr,CLR-method
setMethod(
  f = "transform_alr",
  signature = c(object = "CLR"),
  definition = function(object, j = ncol(object)) {
    D <- ncol(object)
    parts <- object@parts

    ## Reorder (move denominator)
    j <- if (is.character(j)) which(parts == j) else as.integer(j)
    ordering <- c(which(j != seq_len(D)), j)
    parts <- parts[ordering]
    z <- object[, ordering, drop = FALSE]

    ## Compute ratios
    base <- alr_base(D)
    alr <- z %*% base
    rownames(alr) <- rownames(object)
    colnames(alr) <- paste(parts[-D], parts[D], sep = "_")

    .ALR(
      alr,
      parts = parts,
      ratio = paste(parts[-D], parts[D], sep = "/"),
      order = order(ordering),
      base = base,
      weights = object@weights,
      totals = totals(object)
    )
  }
)

#' @export
#' @rdname transform_alr
#' @aliases transform_alr,GroupedCLR-method
setMethod(
  f = "transform_alr",
  signature = c(object = "GroupedCLR"),
  definition = function(object, j = ncol(object), weights = FALSE) {
    z <- methods::callNextMethod()
    .GroupedALR(z, group_indices = group_indices(object),
                group_levels = group_levels(object))
  }
)
