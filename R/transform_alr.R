# DATA TRANSFORMATION: ADDITIVE LOG RATIO
#' @include AllGenerics.R
NULL

# ALR ==========================================================================
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

    base <- diag(1, nrow = D, ncol = D - 1)
    base[D, ] <- -1

    alr <- log(z, base = exp(1)) %*% base
    rownames(alr) <- rownames(object)
    colnames(alr) <- paste(parts[-D], parts[D], sep = "_")

    w <- rep(1 / D, D)
    w <- w[-D] * w[D]

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
