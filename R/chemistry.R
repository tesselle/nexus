# CHEMISTRY
#' @include AllGenerics.R
NULL

is_chemical <- function(object) {
  pattern <- "^([A-Z]{1}[a-z]?[1-9]*)+$"
  z <- grepl(pattern, x = object)
  names(z) <- object
  z
}

#' @export
#' @rdname chemistry
#' @aliases is_oxide,character-method
setMethod(
  f = "is_oxide",
  signature = c(object = "character"),
  definition = function(object) {
    pattern <- "^[A-Z]{1}[a-z]?[1-9]*[O]{1}[1-9]*$"
    z <- grepl(pattern, x = object)
    names(z) <- object
    z
  }
)

#' @export
#' @rdname chemistry
#' @aliases is_oxide,CompositionMatrix-method
setMethod(
  f = "is_oxide",
  signature = c(object = "CompositionMatrix"),
  definition = function(object) {
    methods::callGeneric(colnames(object))
  }
)

#' @export
#' @rdname chemistry
#' @aliases is_element_major,CompositionMatrix-method
setMethod(
  f = "is_element_major",
  signature = c(object = "CompositionMatrix"),
  definition = function(object, min = 1 / 100, max = Inf) {
    .element_threshold(object, min = min, max = max)
  }
)

#' @export
#' @rdname chemistry
#' @aliases is_element_minor,CompositionMatrix-method
setMethod(
  f = "is_element_minor",
  signature = c(object = "CompositionMatrix"),
  definition = function(object, min = 0.1 / 100, max = 1 / 100) {
    .element_threshold(object, min = min, max = max)
  }
)

#' @export
#' @rdname chemistry
#' @aliases is_element_trace,CompositionMatrix-method
setMethod(
  f = "is_element_trace",
  signature = c(object = "CompositionMatrix"),
  definition = function(object, min = -Inf, max = 0.1 / 100) {
    .element_threshold(object, min = min, max = max)
  }
)

.element_threshold <- function(x, min = -Inf, max = Inf) {
  x <- mean(x)
  x >= min & x < max
}
