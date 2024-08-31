# DATA TRANSFORMATION: PIVOT LOG RATIO
#' @include AllGenerics.R
NULL

# Pivot ========================================================================
#' @export
#' @rdname transform_plr
#' @aliases transform_plr,CompositionMatrix-method
setMethod(
  f = "transform_plr",
  signature = c(object = "CompositionMatrix"),
  definition = function(object, pivot = 1) {
    J <- ncol(object)
    parts <- colnames(object)

    ## Reorder
    pivot <- if (is.character(pivot)) which(parts == pivot) else as.integer(pivot)
    ordering <- c(pivot, which(pivot != seq_len(J)))
    parts <- parts[ordering]
    obj <- object[, ordering, drop = FALSE]

    x <- seq_len(J - 1)
    balances <- diag(sqrt((J - x) / (J - x + 1)))
    z <- 1 / matrix(data = seq_len(J) - J, nrow = J, ncol = J)
    z[lower.tri(z)] <- 0
    diag(z) <- 1
    z <- z[-nrow(z), ]

    H <- t(balances %*% z)
    plr <- log(obj, base = exp(1)) %*% H

    ratio <- vapply(
      X = seq_len(J - 1),
      FUN = function(i, parts) {
        j <- length(parts)
        sprintf("%s/(%s)", parts[1], paste0(parts[(i+1):j], collapse = ","))
      },
      FUN.VALUE = character(1),
      parts = parts
    )
    colnames(plr) <- paste0("Z", seq_len(J - 1))
    rownames(plr) <- rownames(object)

    .PLR(
      plr,
      parts = parts,
      ratio = ratio,
      order = order(ordering),
      base = H,
      weights = rep(1 / J, J),
      totals = total(object),
      groups = get_groups(object)
    )
  }
)
