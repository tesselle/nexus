# SPLIT
#' @include AllGenerics.R
NULL

# Split ========================================================================
#' @export
#' @method split CompositionMatrix
split.CompositionMatrix <- function(x, f, drop = FALSE, ...) {
  lapply(
    X = split(x = seq_len(nrow(x)), f = f, drop = drop, sep = "_", ...),
    FUN = function(ind) x[ind, , drop = FALSE]
  )
}

#' @export
#' @rdname split
#' @aliases split,CompositionMatrix-method
setMethod("split", "CompositionMatrix", split.CompositionMatrix)

#' @export
#' @method split LogRatio
split.LogRatio <- function(x, f, drop = FALSE, ...) {
  lapply(
    X = split(x = seq_len(nrow(x)), f = f, drop = drop, sep = "_", ...),
    FUN = function(ind) x[ind, , drop = FALSE]
  )
}

#' @export
#' @rdname split
#' @aliases split,LogRatio-method
setMethod("split", "LogRatio", split.LogRatio)

# Bind =========================================================================
#' @export
#' @rdname bind
#' @aliases rbind2,CompositionMatrix,CompositionMatrix-method
setMethod(
  f = "rbind2",
  signature = c(x = "CompositionMatrix", y = "CompositionMatrix"),
  definition = function(x, y) {
    mtx_x <- methods::as(x, "matrix")
    mtx_y <- methods::as(y, "matrix")

    spl <- c(rownames(x), rownames(y))
    if (any(duplicated(spl))) {
      warning("Duplicated rownames!", call. = FALSE)
      spl <- make.unique(spl, sep = "_")
    }

    z <- rbind(mtx_x, mtx_y)
    rownames(z) <- spl

    .CompositionMatrix(
      z,
      totals = c(total(x), total(y)),
      groups = c(group(x), group(y))
    )
  }
)
