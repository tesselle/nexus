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
