# STATISTICS
#' @include AllGenerics.R AllClasses.R
NULL

#' Geometric Mean
#'
#' @param x A \code{\link{numeric}} vector.
#' @return A \code{\link{numeric}} vector.
#' @keywords internal
#' @noRd
gmean <- function(x) {
  index <- is.finite(x) & x > 0
  exp(mean(log(unclass(x)[index])))
}
