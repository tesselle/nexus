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
    arkhe::assert_colnames(y, colnames(x))

    mtx_x <- methods::as(x, "matrix")
    mtx_y <- methods::as(y, "matrix")

    spl <- c(rownames(x), rownames(y))
    if (any(duplicated(spl))) {
      warning(tr_("Duplicated rownames!"), call. = FALSE)
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
    z <- methods::callNextMethod(x, y)
    group(z, by = c(group_names(x), group_names(y)))
  }
)
