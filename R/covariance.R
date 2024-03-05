# COVARIANCE
#' @include AllGenerics.R
NULL

# Covariance ===================================================================
#' @export
#' @rdname covariance
#' @aliases covariance,CompositionMatrix-method
setMethod(
  f = "covariance",
  signature = c(x = "CompositionMatrix"),
  definition = function(x, center = TRUE, method = "pearson") {
    x <- if (center) transform_clr(x) else transform_alr(x)
    methods::callGeneric(x = x, method = method)
  }
)

#' @export
#' @describeIn covariance Computes the log-ratio covariance matrix
#'  (Aitchison 1986, definition 4.5).
#' @aliases covariance,ALR-method
setMethod(
  f = "covariance",
  signature = c(x = "ALR"),
  definition = function(x, method = "pearson") {
    stats::cov(x, method = method)
  }
)

#' @export
#' @describeIn covariance Computes the centered log-ratio covariance matrix
#'  (Aitchison 1986, definition 4.6).
#' @aliases covariance,ALR-method
setMethod(
  f = "covariance",
  signature = c(x = "CLR"),
  definition = function(x, method = "pearson") {
    stats::cov(x, method = method)
  }
)
