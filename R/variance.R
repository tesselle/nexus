# VARIANCE
#' @include AllGenerics.R
NULL

# Variance =====================================================================
#' @export
#' @rdname variance
#' @aliases variance,LogRatio-method
setMethod(
  f = "variance",
  signature = c("LogRatio"),
  definition = function(x, row_weights = NULL, column_weights = TRUE) {
    n <- nrow(x)
    m <- ncol(x)

    w_row <- rep(1 / n, n)
    if (length(row_weights) == n) {
      arkhe::assert_length(row_weights, n)
      arkhe::assert_positive(row_weights, strict = FALSE)
      w_row <- row_weights / sum(row_weights) # Sum up to 1
    }

    w_col <- if (isTRUE(column_weights)) weights(x) else rep(1 / m, m)
    if (length(column_weights) == m) {
      arkhe::assert_length(column_weights, m)
      arkhe::assert_positive(column_weights, strict = FALSE)
      w_col <- column_weights / sum(column_weights) # Sum up to 1
    }

    z <- sweep(x, MARGIN = 2, STATS = colSums(x * w_row), FUN = "-")
    z <- colSums(diag(w_row) %*% z^2 %*% diag(w_col))
    names(z) <- colnames(x)
    z
  }
)

# Total variance ===============================================================
#' @export
#' @describeIn variance_total The total variance of compositional data is the
#'  trace of the [centred log-ratio covariance][covariance()] matrix
#'  (i.e. *totvar1* in Aitchison 1997).
#' @aliases variance_total,CompositionMatrix-method
setMethod(
  f = "variance_total",
  signature = c("CompositionMatrix"),
  definition = function(x, sd = FALSE) {
    z <- sum(diag(covariance(x, center = TRUE)))
    if (sd) z <- sqrt((1 / (ncol(x) - 1)) * z)
    z
  }
)

#' @export
#' @describeIn variance_total Computes the total log-ratio variance. This is
#'  identical to the weighted sum-of-squared distances between samples
#'  (i.e. *totvar2* in Aitchison 1997).
#' @aliases variance_total,LogRatio-method
setMethod(
  f = "variance_total",
  signature = c("LogRatio"),
  definition = function(x, row_weights = NULL, column_weights = TRUE) {
    sum(variance(x, row_weights = row_weights, column_weights = column_weights))
  }
)

