# REPLACE ZEROS
#' @include AllGenerics.R
NULL

#' @export
#' @rdname zero
#' @aliases replace_zero,CompositionMatrix-method
setMethod(
  f = "replace_zero",
  signature = c(x = "CompositionMatrix"),
  definition = function(x, value = NULL, delta = 2/3) {
    ## Validation
    D <- ncol(x)
    if (is.null(value)) return(x)
    if (length(value) == 1) rep(value, D)
    if (length(value) > 1) arkhe::assert_length(value, D)
    if (length(delta) > 1) arkhe::assert_length(delta, D)

    sigma <- value * delta
    repl <- apply(X = x, MARGIN = 1, FUN = zero_multiplicative, sigma = sigma)

    methods::initialize(x, t(repl))
  }
)

zero_additive <- function(x, sigma) {
  D <- length(x)

  is_zero <- x == 0
  Z <- sum(is_zero)

  x[is_zero] <- (sigma * (Z + 1) * (D - Z)) / D^2
  x[!is_zero] <- x[!is_zero] - (sigma * (Z + 1) * Z) / D^2

  x
}
zero_multiplicative <- function(x, sigma) {
  D <- length(x)

  is_zero <- x == 0

  x[is_zero] <- sigma[is_zero]
  x[!is_zero] <- x[!is_zero] * (1 - (sum(sigma[is_zero])) / 1)

  x
}
