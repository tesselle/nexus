# REPLACE
#' @include AllGenerics.R
NULL

# Zeros ========================================================================
#' @export
#' @rdname replace_zero
#' @aliases replace_zero,CompositionMatrix-method
setMethod(
  f = "replace_zero",
  signature = c(x = "CompositionMatrix"),
  definition = function(x, value, delta = 2/3) {
    ## Validation
    D <- ncol(x)
    if (is.null(value)) return(x)
    if (length(value) == 1) value <- rep(value, D)
    if (length(value) > 1) arkhe::assert_length(value, D)
    if (length(delta) > 1) arkhe::assert_length(delta, D)

    sigma <- value * delta
    r <- apply(X = x, MARGIN = 1, FUN = zero_multiplicative, sigma = sigma)

    methods::initialize(x, t(r))
  }
)

# zero_additive <- function(x, sigma) {
#   D <- length(x)
#
#   is_zero <- x == 0
#   Z <- sum(is_zero)
#
#   x[is_zero] <- (sigma * (Z + 1) * (D - Z)) / D^2
#   x[!is_zero] <- x[!is_zero] - (sigma * (Z + 1) * Z) / D^2
#
#   x
# }
zero_multiplicative <- function(x, sigma) {
  is_zero <- x == 0 & !is.na(x)
  x[is_zero] <- sigma[is_zero]
  x[!is_zero] <- x[!is_zero] * (1 - sum(sigma[is_zero]) / 1)
  x
}

# Missing values ===============================================================
#' @export
#' @rdname replace_NA
#' @aliases replace_NA,CompositionMatrix-method
setMethod(
  f = "replace_NA",
  signature = c(x = "CompositionMatrix"),
  definition = function(x, value) {
    ## Validation
    D <- ncol(x)
    if (is.null(value)) return(x)
    if (length(value) == 1) value <- rep(value, D)
    if (length(value) > 1) arkhe::assert_length(value, D)

    r <- apply(X = x, MARGIN = 1, FUN = missing_multiplicative, sigma = value)

    methods::initialize(x, t(r))
  }
)

missing_multiplicative <- function(x, sigma) {
  is_missing <- is.na(x)
  x[is_missing] <- sigma[is_missing]
  x[!is_missing] <- x[!is_missing] * (1 - sum(sigma[is_missing])) / sum(x[!is_missing])
  x
}
