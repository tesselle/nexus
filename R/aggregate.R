# AGGREGATE
#' @include AllGenerics.R
NULL

# Aggregate ====================================================================
#' @export
#' @method aggregate CompositionMatrix
aggregate.CompositionMatrix <- function(x, by, FUN, ...,
                                        simplify = TRUE, drop = TRUE) {
  m <- nrow(x)

  ## Grouping
  index <- as_groups(by)
  if (nlevels(index) == 0 || nlevels(index) == m) {
    warning("Nothing to group by.", call. = FALSE)
    return(x)
  }

  aggr <- tapply(
    X = seq_len(m),
    INDEX = index,
    FUN = function(i, data, fun, ...) fun(data[i, , drop = FALSE], ...),
    data = x,
    fun = FUN,
    ...,
    simplify = FALSE
  )

  has_dim <- vapply(
    X = aggr,
    FUN = function(x) !is.null(nrow(x)) && nrow(x) > 1,
    FUN.VALUE = logical(1)
  )

  if (any(has_dim) || !simplify) return(aggr)
  do.call(rbind, aggr)
}

#' @export
#' @rdname aggregate
#' @aliases aggregate,CompositionMatrix-method
setMethod("aggregate", "CompositionMatrix", aggregate.CompositionMatrix)
