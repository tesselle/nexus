# COMPOSITIONAL MEAN
#' @include AllGenerics.R
NULL

#' @export
#' @rdname condense
#' @aliases condense,CompositionMatrix-method
setMethod(
  f = "condense",
  signature = c("CompositionMatrix"),
  definition = function(x, by, ...) {
    m <- nrow(x)

    ## Grouping
    by <- get_variable(x, which = by)
    arkhe::assert_length(by, m)
    by <- as.factor(by)

    z <- tapply(
      X = seq_len(m),
      INDEX = by,
      FUN = function(i, data, ...) {
        mean(data[i, , drop = FALSE], ...)
      },
      data = x,
      ...,
      simplify = FALSE
    )
    z <- do.call(rbind, z)

    tot <- tapply(X = get_totals(x), INDEX = by, FUN = mean, simplify = TRUE)

    rownames(z) <- levels(by)
    .CompositionMatrix(z, totals = as.numeric(tot))
  }
)
