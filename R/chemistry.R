# CHEMISTRY
#' @include AllGenerics.R
NULL

#' @export
#' @rdname element
#' @aliases element_major,CompositionMatrix-method
setMethod(
  f = "element_major",
  signature = c(object = "CompositionMatrix"),
  definition = function(object, min = 1 / 100, max = Inf) {
    .element_threshold(object, min = min, max = max)
  }
)

#' @export
#' @rdname element
#' @aliases element_minor,CompositionMatrix-method
setMethod(
  f = "element_minor",
  signature = c(object = "CompositionMatrix"),
  definition = function(object, min = 0.1 / 100, max = 1 / 100) {
    .element_threshold(object, min = min, max = max)
  }
)

#' @export
#' @rdname element
#' @aliases element_trace,CompositionMatrix-method
setMethod(
  f = "element_trace",
  signature = c(object = "CompositionMatrix"),
  definition = function(object, min = -Inf, max = 0.1 / 100) {
    .element_threshold(object, min = min, max = max)
  }
)

.element_threshold <- function(x, min = -Inf, max = Inf) {
  x <- mean(x)
  x >= min & x < max
}
