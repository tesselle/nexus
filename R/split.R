# SPLIT
#' @include AllGenerics.R
NULL

#' @export
#' @method split CompositionMatrix
split.CompositionMatrix <- function(x, f, drop = FALSE, ...) {
  lapply(
    X = split(x = seq_len(nrow(x)), f = f, drop = drop, sep = "_", ...),
    FUN = function(ind) x[ind, , drop = FALSE]
  )
}

#' @export
#' @rdname group_split
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
#' @rdname group_split
#' @aliases split,LogRatio-method
setMethod("split", "LogRatio", split.LogRatio)

#' @export
#' @rdname group_split
#' @aliases group_split,GroupedComposition-method
setMethod(
  f = "group_split",
  signature = "GroupedComposition",
  definition = function(object, ...) {
    lapply(
      X = group_rows(object),
      FUN = function(ind) ungroup(object[ind, , drop = FALSE])
    )
  }
)

#' @export
#' @rdname group_split
#' @aliases group_split,GroupedLogRatio-method
setMethod(
  f = "group_split",
  signature = "GroupedLogRatio",
  definition = function(object, ...) {
    lapply(
      X = group_rows(object),
      FUN = function(ind) ungroup(object[ind, , drop = FALSE])
    )
  }
)
