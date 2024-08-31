# COMPOSITIONAL MEAN
#' @include AllGenerics.R
NULL

#' @export
#' @rdname condense
#' @aliases condense,CompositionMatrix-method
setMethod(
  f = "condense",
  signature = c("CompositionMatrix"),
  definition = function(x, by = group(x), ...) {
    m <- nrow(x)

    ## Grouping
    if (!has_groups(by)) return(x) # Nothing to do
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

    tot <- tapply(X = total(x), INDEX = by, FUN = mean, simplify = TRUE)
    grp <- group(x)
    if (has_groups(grp)) grp <- flatten_chr(x = grp, by = by)
    else grp <- rep(NA_character_, length(tot))

    rownames(z) <- levels(by)
    .CompositionMatrix(z, totals = as.numeric(tot), groups = grp)
  }
)
