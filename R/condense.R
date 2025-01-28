# COMPOSITIONAL MEAN
#' @include AllGenerics.R
NULL

#' @export
#' @rdname condense
#' @aliases condense,CompositionMatrix-method
setMethod(
  f = "condense",
  signature = "CompositionMatrix",
  definition = function(x, by, ignore_na = FALSE, ignore_zero = TRUE,
                        verbose = FALSE, ...) {
    x <- group(x, by = by)
    y <- methods::callGeneric(x = x, ignore_na = ignore_na,
                              ignore_zero = ignore_zero, verbose = verbose, ...)
    ungroup(y)
  }
)

#' @export
#' @rdname condense
#' @aliases condense,GroupedComposition-method
setMethod(
  f = "condense",
  signature = "GroupedComposition",
  definition = function(x, by = NULL, ignore_na = FALSE, ignore_zero = TRUE,
                        verbose = FALSE, ...) {
    ## Grouping
    grp <- group_factor(x)
    if (!is.null(by)) x <- group(x, by = by, verbose = verbose)

    ## Compute mean
    z <- aggregate(
      x = x,
      FUN = mean,
      ignore_na = ignore_na,
      ignore_zero = ignore_zero,
      simplify = TRUE
    )
    tot <- tapply(X = totals(x), INDEX = group_factor(x), FUN = mean)

    z <- .CompositionMatrix(z, totals = as.numeric(tot))
    group(z, by = flatten_chr(x = grp, by = group_factor(x)), verbose = verbose)
  }
)

flatten_chr <- function(x, by) {
  x <- as.character(x)
  z <- tapply(X = x, INDEX = by, FUN = unique, simplify = FALSE)
  z <- vapply(X = z, FUN = paste0, FUN.VALUE = character(1), collapse = ":")
  z
}
