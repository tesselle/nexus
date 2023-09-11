# OPERATORS
#' @include AllGenerics.R
NULL

# Closure ======================================================================
#' @export
#' @rdname closure
#' @aliases closure,numeric-method
setMethod(
  f = "closure",
  signature = c(x = "numeric"),
  definition = function(x, total = 1, na.rm = FALSE) {
    x * total / sum(x, na.rm = na.rm)
  }
)

#' @export
#' @rdname closure
#' @aliases closure,matrix-method
setMethod(
  f = "closure",
  signature = c(x = "matrix"),
  definition = function(x, total = 1, na.rm = FALSE) {
    x * total / rowSums(x, na.rm = na.rm)
  }
)

# Perturbation =================================================================
#' @export
#' @rdname arithmetic
#' @aliases `+`,CompositionMatrix,CompositionMatrix-method
setMethod(
  f = "+",
  signature = c(e1 = "CompositionMatrix", e2 = "CompositionMatrix"),
  definition = function (e1, e2) {
    arkhe::assert_dimensions(e2, dim(e1))
    z <- e1 * e2
    z <- as_composition(z)
    set_samples(z) <- get_samples(e1)
    set_groups(z) <- get_groups(e1)
    z
  }
)

#' @export
#' @rdname arithmetic
#' @aliases `-`,CompositionMatrix,CompositionMatrix-method
setMethod(
  f = "-",
  signature = c(e1 = "CompositionMatrix", e2 = "CompositionMatrix"),
  definition = function (e1, e2) {
    arkhe::assert_dimensions(e2, dim(e1))
    z <- e1 / e2
    z <- as_composition(z)
    set_samples(z) <- get_samples(e1)
    set_groups(z) <- get_groups(e1)
    z
  }
)

#' @export
#' @rdname perturbation
#' @aliases perturbation,numeric,numeric-method
setMethod(
  f = "perturbation",
  signature = c(x = "numeric", y = "numeric"),
  definition = function(x, y) {
    arkhe::assert_length(y, length(x))
    closure(x * y)
  }
)

#' @export
#' @rdname perturbation
#' @aliases perturbation,CompositionMatrix,numeric-method
setMethod(
  f = "perturbation",
  signature = c(x = "CompositionMatrix", y = "numeric"),
  definition = function(x, y) {
    x + as_composition(y)
  }
)

#' @export
#' @rdname perturbation
#' @aliases perturbation,CompositionMatrix,CompositionMatrix-method
setMethod(
  f = "perturbation",
  signature = c(x = "CompositionMatrix", y = "matrix"),
  definition = function(x, y) {
    x + as_composition(y)
  }
)

# Powering =====================================================================
#' @export
#' @rdname arithmetic
#' @aliases `*`,CompositionMatrix,numeric-method
setMethod(
  f = "*",
  signature = c(e1 = "CompositionMatrix", e2 = "numeric"),
  definition = function (e1, e2) {
    arkhe::assert_length(e2, 1L)
    z <- e1 ^ e2
    z <- as_composition(z)
    set_samples(z) <- get_samples(e1)
    set_groups(z) <- get_groups(e1)
    z
  }
)

#' @export
#' @rdname arithmetic
#' @aliases `*`,numeric,CompositionMatrix-method
setMethod(
  f = "*",
  signature = c(e1 = "numeric", e2 = "CompositionMatrix"),
  definition = function (e1, e2) {
    methods::callGeneric(e1 = e2, e2 = e1)
  }
)

#' @export
#' @rdname powering
#' @aliases powering,numeric,numeric-method
setMethod(
  f = "powering",
  signature = c(x = "numeric", a = "numeric"),
  definition = function(x, a) {
    arkhe::assert_length(a, 1L)
    closure(x ^ a)
  }
)

#' @export
#' @rdname powering
#' @aliases powering,numeric,numeric-method
setMethod(
  f = "powering",
  signature = c(x = "CompositionMatrix", a = "numeric"),
  definition = function(x, a) {
    x * a
  }
)
