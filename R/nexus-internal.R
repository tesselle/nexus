# HELPERS

missingORnull <- function(x) {
  missing(x) || is.null(x)
}

#' Geometric Mean
#'
#' @param x A [`numeric`] vector.
#' @param trim A length-one [`numeric`] vector specifying the fraction (0 to 0.5)
#'  of observations to be trimmed from each end of `x` before the mean is
#'  computed.
#' @param na.rm A [`logical`] scalar: should `NA` values be stripped before the
#'  computation proceeds?
#' @param zero.rm A [`logical`] scalar: should zeros be stripped before the
#'  computation proceeds?
#' @return A [`numeric`] vector.
#' @keywords internal
gmean <- function(x, trim = 0, na.rm = FALSE, zero.rm = FALSE) {
  if (na.rm) x <- x[is.finite(x)]
  if (zero.rm) x <- x[x > 0]
  exp(mean(log(x), trim = trim))
}

#' Label Percentages
#'
#' @param x A [`numeric`] vector.
#' @param digits An [`integer`] indicating the number of decimal places.
#'  If `NULL` (the default), breaks will have the minimum number of digits
#'  needed to show the difference between adjacent values.
#' @param trim A [`logical`] scalar. If `FALSE` (the default), values are
#'  right-justified to a common width (see [base::format()]).
#' @return A [`character`] vector.
#' @keywords internal
#' @noRd
label_percent <- function(x, digits = NULL, trim = FALSE) {
  i <- !is.na(x)
  y <- x[i]
  y <- abs(y) * 100
  y <- format(y, trim = trim, digits = digits)
  y <- paste0(y, "%")
  x[i] <- y
  x
}
