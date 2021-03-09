# STATISTICS
#' @include AllGenerics.R AllClasses.R
NULL

#' Geometric Mean
#'
#' @param x A [`numeric`] vector.
#' @return A [`numeric`] vector.
#' @keywords internal
#' @noRd
gmean <- function(x) {
  index <- is.finite(x) & x > 0
  exp(mean(log(unclass(x)[index])))
}

#' Mahalanobis Distance
#'
#' @param object A numeric matrix.
#' @param group A numeric matrix.
#' @param robust A [`logical`] scalar.
#' @param ... Extra parameter to be passed to [transform_pivot()].
#' @return
#'  A [`numeric`] vector giving the squared Mahalanobis distance of all rows in
#'  `object`.
#' @seealso [stats::mahalanobis()]
#' @keywords internal
#' @noRd
stats_mahalanobis <- function(object, group, robust = TRUE, ...) {
  if (missing(group)) group <- object

  ilr_object <- transform_pivot(object, ...)
  ilr_group <- transform_pivot(group, ...)

  x <- as.matrix(ilr_object)
  y <- as.matrix(ilr_group)
  if (robust) {
    # Robust estimators
    v <- robustbase::covMcd(y)
    est <- list(mean = v$center, cov = v$cov)
  } else {
    # Standard estimators
    est <- list(mean = colMeans(y, na.rm = TRUE), cov = stats::cov(y))
  }

  stats::mahalanobis(x, center = est$mean, cov = est$cov)
}
