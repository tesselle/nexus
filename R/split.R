# SPLIT
#' @include AllGenerics.R
NULL

# Split ========================================================================
#' @export
#' @rdname group_split
#' @aliases group_split,CompositionMatrix-method
setMethod(
  f = "group_split",
  signature = "CompositionMatrix",
  definition = function(object, by, ...) {
    x <- group(object, by = by, ...)
    methods::callGeneric(object = x)
  }
)

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
      FUN = function(ind) object[ind, , drop = FALSE]
    )
  }
)

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
      totals = c(totals(x), totals(y))
    )
  }
)
