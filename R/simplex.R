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
#' @aliases `%perturbe%`,CompositionMatrix,CompositionMatrix-method
setMethod(
  f = "%perturbe%",
  signature = c(x = "CompositionMatrix", y = "CompositionMatrix"),
  definition = function(x, y) {
    arkhe::assert_dimensions(y, dim(x))
    if (all(x <= 0)) x <- 1 / x
    if (all(y <= 0)) y <- 1 / y
    z <- x * y
    z <- as_composition(z)
    set_samples(z) <- get_samples(x)
    set_groups(z) <- get_groups(x)
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
    x %perturbe% as_composition(y)
  }
)

#' @export
#' @rdname perturbation
#' @aliases perturbation,CompositionMatrix,CompositionMatrix-method
setMethod(
  f = "perturbation",
  signature = c(x = "CompositionMatrix", y = "matrix"),
  definition = function(x, y) {
    x %perturbe% as_composition(y)
  }
)

# Powering =====================================================================
#' @export
#' @rdname arithmetic
#' @aliases `%power%`,CompositionMatrix,numeric-method
setMethod(
  f = "%power%",
  signature = c(x = "CompositionMatrix", y = "numeric"),
  definition = function(x, y) {
    arkhe::assert_length(y, 1L)
    z <- x ^ y
    z <- as_composition(z)
    set_samples(z) <- get_samples(x)
    set_groups(z) <- get_groups(x)
    z
  }
)

#' @export
#' @rdname arithmetic
#' @aliases `%power%`,numeric,CompositionMatrix-method
setMethod(
  f = "%power%",
  signature = c(x = "numeric", y = "CompositionMatrix"),
  definition = function(x, y) {
    methods::callGeneric(x = y, y = x)
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
    x %power% a
  }
)

# Scalar product ===============================================================
#' @export
#' @rdname scalar
#' @aliases scalar,numeric,numeric-method
setMethod(
  f = "scalar",
  signature = c(x = "numeric", y = "numeric"),
  definition = function(x, y) {
    n <- length(x)
    arkhe::assert_length(y, n)

    D <- seq_len(n)
    z <- 0
    for (i in D) {
      j <- utils::tail(D, -i)
      z <- z + sum(log(x[i] / x[j]) * log(y[i] / y[j]))
    }
    (1 / n) * z
  }
)

#' @export
#' @rdname scalar
#' @aliases scalar,CompositionMatrix,CompositionMatrix-method
setMethod(
  f = "scalar",
  signature = c(x = "CompositionMatrix", y = "CompositionMatrix"),
  definition = function(x, y) {
    arkhe::assert_dimensions(y, dim(x))
    m <- nrow(x)

    z <- numeric(m)
    for (i in seq_len(m)) {
      z[i] <- scalar(x[i, ], y[i, ])
    }
    z
  }
)

#' Norm of a Composition
#'
#' @param x A [`CompositionMatrix-class`] object.
#' @return A [`numeric`] vector.
#' @keywords internal
#' @noRd
norm <- function(x) {
  sqrt(scalar(x, x))
}

#' Aitchison Distance
#'
#' @param x A [`CompositionMatrix-class`] object.
#' @param diag A [`logical`] scalar: should the diagonal of the distance matrix
#'  be printed?
#' @param upper A [`logical`] scalar: should the upper triangle of the distance
#'  matrix be printed?
#' @return A [`dist`] object.
#' @keywords internal
#' @noRd
aitchison <- function(x, diag = FALSE, upper = FALSE) {
  m <- nrow(x)
  spl <- rownames(x)

  d <- utils::combn(
    x = seq_len(m),
    m = 2,
    FUN = function(i, coda) {
      x <- coda[i[1], ]
      y <- coda[i[2], ]
      norm(x / y)
    },
    coda = x
  )

  ## Matrix of results
  mtx <- matrix(data = 0, nrow = m, ncol = m, dimnames = list(spl, spl))
  mtx[lower.tri(mtx, diag = FALSE)] <- d
  mtx <- t(mtx)
  mtx[lower.tri(mtx, diag = FALSE)] <- d

  stats::as.dist(mtx, diag = diag, upper = upper)
}
