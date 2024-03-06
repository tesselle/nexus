# COMPOSITIONAL MEAN
#' @include AllGenerics.R
NULL

#' @export
#' @rdname flatten
#' @aliases flatten,CompositionMatrix-method
setMethod(
  f = "flatten",
  signature = c("CompositionMatrix"),
  definition = function(x, by = get_samples(x), na.rm = FALSE) {
    m <- nrow(x)

    arkhe::assert_length(by, m)
    by <- factor(x = by, levels = unique(by)) # Keep original ordering

    z <- tapply(
      X = seq_len(m),
      INDEX = by,
      FUN = function(i, data, na.rm) {
        mean(data[i, , drop = FALSE], na.rm = na.rm)
      },
      data = x,
      na.rm = na.rm,
      simplify = FALSE
    )
    z <- do.call(rbind, z)

    tot <- tapply(X = get_totals(x), INDEX = by, FUN = mean, simplify = TRUE)
    lab <- flatten_chr(x = get_identifiers(x), by = by)
    spl <- flatten_chr(x = get_samples(x), by = by)
    grp <- flatten_chr(x = get_groups(x), by = by)

    .CompositionMatrix(z, totals = as.numeric(tot),
                       codes = lab, samples = spl, groups = grp)
  }
)

flatten_chr <- function(x, by) {
  z <- tapply(X = x, INDEX = by, FUN = unique, simplify = FALSE)
  z <- vapply(X = z, FUN = paste0, FUN.VALUE = character(1), collapse = ":")
  z
}
