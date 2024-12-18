# COMPOSITIONAL MEAN
#' @include AllGenerics.R
NULL

#' @export
#' @rdname condense
#' @aliases condense,CompositionMatrix-method
setMethod(
  f = "condense",
  signature = "CompositionMatrix",
  definition = function(x, by, verbose = getOption("nexus.verbose"), ...) {
    x <- group(x, by = by)
    methods::callGeneric(x = x, verbose = verbose, ...)
  }
)

#' @export
#' @rdname condense
#' @aliases condense,GroupedComposition-method
setMethod(
  f = "condense",
  signature = "GroupedComposition",
  definition = function(x, by = NULL, verbose = getOption("nexus.verbose"), ...) {
    ## Grouping
    grp <- group_factor(x)
    if (!is.null(by)) x <- group(x, by = by, verbose = verbose)

    ## Compute mean
    z <- aggregate(x, FUN = mean, ..., simplify = TRUE)
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
