# BIND
#' @include AllGenerics.R
NULL

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

    .CompositionMatrix(z, totals = c(totals(x), totals(y)))
  }
)

#' @export
#' @rdname bind
#' @aliases rbind2,GroupedComposition,GroupedComposition-method
setMethod(
  f = "rbind2",
  signature = c(x = "GroupedComposition", y = "GroupedComposition"),
  definition = function(x, y) {
    stop("Combining grouped matrices by rows is not currently supported.",
         call. = FALSE)
  }
)
