# STATISTICS
#' @include AllGenerics.R
NULL

# Metric variance ==============================================================
#' @export
#' @rdname metric_var
#' @aliases metric_var,CompositionMatrix-method
setMethod(
  f = "metric_var",
  signature = c("CompositionMatrix"),
  definition = function(x) {
    sum(diag(covariance(x, center = TRUE)))
  }
)

# Metric standard deviation ====================================================
#' @export
#' @rdname metric_var
#' @aliases metric_sd,CompositionMatrix-method
setMethod(
  f = "metric_sd",
  signature = c("CompositionMatrix"),
  definition = function(x) {
    sqrt((1 / (ncol(x) - 1)) * metric_var(x))
  }
)
