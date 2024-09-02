# AGGREGATE
#' @include AllGenerics.R
NULL

# Aggregate ====================================================================
#' @export
#' @method aggregate CompositionMatrix
aggregate.CompositionMatrix <- function(x, by, FUN, ...,
                                        simplify = TRUE, drop = TRUE) {
  m <- nrow(x)

  ## Validation
  if (!is.list(by)) by <- list(by)
  arkhe::assert_lengths(by, m)

  ## Grouping
  index <- interaction(by, drop = drop, sep = "_")
  if (length(unique(index)) == m) {
    warning("Nothing to group by.", call. = FALSE)
    return(x)
  }

  m <- tapply(
    X = seq_len(m),
    INDEX = index,
    FUN = function(i, data, fun, ...) fun(data[i, , drop = FALSE], ...),
    data = x,
    fun = FUN,
    ...,
    simplify = FALSE
  )

  has_dim <- vapply(
    X = m,
    FUN = function(x) !is.null(nrow(x)) && nrow(x) > 1,
    FUN.VALUE = logical(1)
  )

  if (any(has_dim) || !simplify) return(m)
  do.call(rbind, m)
}

#' @export
#' @rdname aggregate
#' @aliases aggregate,CompositionMatrix-method
setMethod("aggregate", "CompositionMatrix", aggregate.CompositionMatrix)
