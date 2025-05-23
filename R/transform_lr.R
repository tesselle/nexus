# DATA TRANSFORMATION: LOG RATIO
#' @include AllGenerics.R
NULL

# LR ===========================================================================
#' @export
#' @rdname transform_lr
#' @aliases transform_lr,CompositionMatrix-method
setMethod(
  f = "transform_lr",
  signature = c(object = "CompositionMatrix"),
  definition = function(object, weights = FALSE) {
    J <- ncol(object)
    parts <- colnames(object)

    ## Compute weights
    weights <- make_weights(object, weights = weights)

    ## Computes ratios
    jj <- utils::combn(seq_len(J), 2, simplify = FALSE)
    lr <- matrix(data = 0, nrow = nrow(object), ncol = length(jj))
    for (i in seq_along(jj)) {
      a <- jj[[i]][[1]]
      b <- jj[[i]][[2]]
      r <- object[, a, drop = TRUE] / object[, b, drop = TRUE]
      lr[, i] <- log(r, base = exp(1))
    }

    ## Make names
    ratio <- unlist(utils::combn(parts, 2, FUN = paste, collapse = "/", simplify = FALSE))
    rownames(lr) <- rownames(object)
    colnames(lr) <- ratio

    .LR(
      lr,
      parts = parts,
      ratio = ratio,
      order = seq_len(J),
      weights = weights,
      totals = totals(object)
    )
  }
)

#' @export
#' @rdname transform_lr
#' @aliases transform_lr,GroupedComposition-method
setMethod(
  f = "transform_lr",
  signature = c(object = "GroupedComposition"),
  definition = function(object, weights = FALSE) {
    z <- methods::callNextMethod()
    .GroupedLR(z, group_indices = group_indices(object),
               group_levels = group_levels(object),
               group_ordered = is_ordered(object))
  }
)
