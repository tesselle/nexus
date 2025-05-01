# AGGREGATE
#' @include AllGenerics.R
NULL

# Aggregate ====================================================================
#' @export
#' @method aggregate CompositionMatrix
aggregate.CompositionMatrix <- function(x, by, FUN, ...,
                                        simplify = TRUE, drop = TRUE) {
  x <- group(x, by = by, drop_levels = drop, verbose = FALSE)
  aggregate(x, FUN, ..., simplify = simplify)
}

#' @export
#' @rdname aggregate
#' @aliases aggregate,CompositionMatrix-method
setMethod("aggregate", "CompositionMatrix", aggregate.CompositionMatrix)

#' @export
#' @method aggregate GroupedComposition
aggregate.GroupedComposition <- function(x, FUN, ..., simplify = TRUE) {
  ## Grouping
  aggr <- lapply(
    X = group_rows(x),
    FUN = function(i, data, fun, ...) fun(data[i, , drop = FALSE], ...),
    data = x,
    fun = FUN,
    ...
  )

  has_dim <- vapply(
    X = aggr,
    FUN = function(x) !is.null(nrow(x)) && nrow(x) > 1,
    FUN.VALUE = logical(1)
  )

  if (any(has_dim) || !simplify) return(aggr)
  aggr <- do.call(rbind, aggr)
  rownames(aggr) <- group_levels(x)
  aggr
}

#' @export
#' @rdname aggregate
#' @aliases aggregate,GroupedComposition-method
setMethod("aggregate", "GroupedComposition", aggregate.GroupedComposition)
