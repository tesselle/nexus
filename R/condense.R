# COMPOSITIONAL MEAN
#' @include AllGenerics.R
NULL

#' @export
#' @rdname condense
#' @aliases condense,CompositionMatrix-method
setMethod(
  f = "condense",
  signature = c("CompositionMatrix"),
  definition = function(x, by = groups(x), ...) {
    m <- nrow(x)

    ## Grouping
    index <- as_groups(by)
    if (nlevels(index) == 0 || nlevels(index) == m) {
      warning("Nothing to group by.", call. = FALSE)
      return(x)
    }

    z <- tapply(
      X = seq_len(m),
      INDEX = index,
      FUN = function(i, data, ...) {
        mean(data[i, , drop = FALSE], ...)
      },
      data = x,
      ...,
      simplify = FALSE
    )
    z <- do.call(rbind, z)

    tot <- tapply(X = totals(x), INDEX = index, FUN = mean, simplify = TRUE)
    grp <- groups(x)

    if (has_groups(grp)) grp <- flatten_chr(x = grp, by = index)
    else grp <- rep(NA, length(tot))

    rownames(z) <- levels(index)
    .CompositionMatrix(z, totals = as.numeric(tot), groups = as_groups(grp))
  }
)
