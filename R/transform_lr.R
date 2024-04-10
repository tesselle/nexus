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
  definition = function(object) {
    J <- ncol(object)
    parts <- colnames(object)
    weights <- rep(1 / J, J)

    w <- unlist(utils::combn(weights, 2, FUN = function(x) Reduce(`*`, x),
                             simplify = FALSE))
    r <- unlist(utils::combn(parts, 2, FUN = paste, collapse = "_",
                             simplify = FALSE))

    jj <- utils::combn(seq_len(J), 2, simplify = FALSE)
    lr <- matrix(data = 0, nrow = nrow(object), ncol = length(jj))
    for (i in seq_along(jj)) {
      a <- jj[[i]][[1]]
      b <- jj[[i]][[2]]
      lr[, i] <- log(object[, a] / object[, b], base = exp(1))
    }

    rownames(lr) <- rownames(object)
    colnames(lr) <- r

    .LR(
      lr,
      parts = parts,
      ratio = r,
      order = seq_len(J),
      weights = w,
      totals = object@totals,
      samples = object@samples,
      groups = object@groups
    )
  }
)
