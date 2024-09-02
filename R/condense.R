# COMPOSITIONAL MEAN
#' @include AllGenerics.R
NULL

#' @export
#' @rdname condense
#' @aliases condense,CompositionMatrix-method
setMethod(
  f = "condense",
  signature = c("CompositionMatrix"),
  definition = function(x, by = groups(x), drop = TRUE, ...) {
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
    else grp <- rep(NA_character_, length(tot))

    rownames(z) <- levels(index)
    .CompositionMatrix(z, totals = as.numeric(tot), groups = grp)
  }
)
