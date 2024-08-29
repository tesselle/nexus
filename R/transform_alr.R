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
  definition = function(object, j = ncol(object)) {
    D <- ncol(object)
    parts <- colnames(object)

    ## Reorder
    j <- if (is.character(j)) which(parts == j) else as.integer(j)
    ordering <- c(which(j != seq_len(D)), j)
    parts <- parts[ordering]
    z <- object[, ordering, drop = FALSE]

    base <- alr_base(D)

    alr <- log(z, base = exp(1)) %*% base
    rownames(alr) <- rownames(object)
    colnames(alr) <- paste(parts[-D], parts[D], sep = "_")

    w <- rep(1 / D, D)
    w <- w[-1] * w[1]

    .ALR(
      alr,
      parts = parts,
      ratio = colnames(alr),
      order = order(ordering),
      base = base,
      weights = w,
      totals = get_totals(object),
      groups = get_groups(object)
    )
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

    ## Reorder
    j <- if (is.character(j)) which(parts == j) else as.integer(j)
    ordering <- c(which(j != seq_len(D)), j)
    parts <- parts[ordering]
    z <- object[, ordering, drop = FALSE]

    base <- alr_base(D)

    alr <- z %*% base
    rownames(alr) <- rownames(object)
    colnames(alr) <- paste(parts[-D], parts[D], sep = "_")

    w <- rep(1 / D, D)
    w <- w[-1] * w[1]

    .ALR(
      alr,
      parts = parts,
      ratio = colnames(alr),
      order = order(ordering),
      base = base,
      weights = w,
      totals = get_totals(object),
      groups = get_groups(object)
    )
  }
)
