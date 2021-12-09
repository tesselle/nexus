# DATA TRANSFORMATION
#' @include AllClasses.R AllGenerics.R
NULL

# CLR ==========================================================================
#' @export
#' @rdname centered-log-ratio
#' @aliases transform_clr,CompositionMatrix-method
setMethod(
  f = "transform_clr",
  signature = signature(object = "CompositionMatrix"),
  definition = function(object, weights = FALSE) {
    D <- ncol(object)
    parts <- colnames(object)

    w <- if (any(weights)) colMeans(object) else rep(1 / D, D)
    if (is.numeric(weights)) {
      if (length(weights) != D) stop()
      if (any(weights <= 0)) stop()
      w <- weights / sum(weights) # Sum up to 1
    }

    clr <- sweep(log(object), 1, rowSums(log(object) %*% diag(w)))
    dimnames(clr) <- dimnames(object)

    .CLR(clr, parts = parts, weights = w)
  }
)

# ALR ==========================================================================
#' @export
#' @rdname additive-log-ratio
#' @aliases transform_alr,CompositionMatrix-method
setMethod(
  f = "transform_alr",
  signature = signature(object = "CompositionMatrix"),
  definition = function(object, j = ncol(object), weights = FALSE) {
    D <- ncol(object)
    parts <- colnames(object)
    j <- if (is.character(j)) which(parts == j) else as.integer(j)
    d <- object[, j, drop = TRUE]

    alr <- log(object[, -j, drop = FALSE] / d, base = exp(1))
    dimnames(alr) <- list(rownames(object),
                          paste(parts[-j], parts[j], sep = "_"))

    w <- if (any(weights)) colMeans(object) else rep(1 / D, D)
    if (is.numeric(weights)) {
      if (length(weights) != D) stop()
      if (any(weights <= 0)) stop()
      w <- weights / sum(weights) # Sum up to 1
    }
    w <- w[-j] * w[j]

    .ALR(alr, parts = parts, weights = w, denominator = j)
  }
)

# ILR ==========================================================================
#' @export
#' @rdname isometric-log-ratio
#' @aliases transform_ilr,CompositionMatrix-method
setMethod(
  f = "transform_ilr",
  signature = signature(object = "CompositionMatrix"),
  definition = function(object) {
    ilr <- ilr_basic(object)
    .ILR(ilr$ilr, parts = ilr$parts, ratio = ilr$ratio, base = ilr$base)
  }
)

#' Basic ILR
#'
#' @param x A [`numeric`] [`matrix`] of compositional data.
#' @details
#'  Uses the orthonormal basis (Helmert matrix) originally defined by Egozcue
#'  *et al.* (2003).
#' @keywords internal
#' @noRd
ilr_basic <- function(x) {
  D <- ncol(x)
  seq_parts <- seq_len(D - 1)
  parts <- colnames(x)

  ## Helmert matrix (rotation matrix)
  H <- stats::contr.helmert(D)                  # D x D-1
  H <- t(H) / sqrt((seq_parts + 1) * seq_parts) # D-1 x D

  ## Center
  m <- diag(x = 1, nrow = D) - matrix(data = 1 / D, nrow = D, ncol = D)
  H <- tcrossprod(m, H)

  ## Rotated and centered values
  y <- log(x, base = exp(1))
  ilr <- y %*% H

  ilr_ratio <- vapply(
    X = seq_parts,
    FUN = function(i, k) {
      paste(paste0(k[seq_len(i)], collapse = "-"), k[i + 1], sep = "_")
    },
    FUN.VALUE = character(1),
    k = parts
  )
  colnames(ilr) <- paste0("Z", seq_parts)
  rownames(ilr) <- rownames(x)

  list(ilr = ilr, parts = parts, ratio = ilr_ratio,
       weights = rep(1 / D, D), base = H)
}

#' Single ILR
#'
#' @param x A [`numeric`] [`matrix`] of compositional data.
#' @param n An [`integer`] vector specifying the parts in the numerator.
#' @param d An [`integer`] vector specifying the parts in the denominator.
#' @param weights A [`logical`] scalar: sould a varying weight be used. If
#'  `FALSE` (the default), equally-weighted parts are used. Alternatively, a
#'  positive [`numeric`] vector of weights can be specified.
#' @keywords internal
#' @noRd
ilr_single <- function(x, n, d, weights = FALSE) {
  D <- ncol(x)
  parts <- colnames(x)

  ## Validation
  if (missing(n)) n <- 1
  if (missing(d)) d <- seq_len(D)[-n]
  if (length(intersect(n, d)) > 0)
    stop("Numerator and denominator must not intersect.", call. = FALSE)

  w <- if (any(weights)) colMeans(x) else rep(1 / D, D)
  if (is.numeric(weights)) {
    if (length(weights) != D) stop()
    if (any(weights <= 0)) stop()
    w <- weights / sum(weights) # Sum up to 1
  }
  num_w <- sum(w[n])
  den_w <- sum(w[d])

  num <- log(x[, n, drop = FALSE]) %*% diag(w[n], nrow = length(w[n]))
  num <- rowSums(num) / num_w
  den <- log(x[, d, drop = FALSE]) %*% diag(w[d], nrow = length(w[d]))
  den <- rowSums(den) / den_w

  ilr <- sqrt(num_w * den_w / (num_w + den_w) * length(c(n, d))) * (num - den)
  ilr_weights <- num_w * den_w
  ilr_ratio <- paste(
    paste0(parts[n], collapse = "-"),
    paste0(parts[d], collapse = "-"),
    sep = "_"
  )

  list(ilr = ilr, parts = parts, ratio = ilr_ratio, weights = ilr_weights)
}

# Pivot ========================================================================
#' @export
#' @rdname pivot-log-ratio
#' @aliases transform_pivot,CompositionMatrix-method
setMethod(
  f = "transform_pivot",
  signature = signature(object = "CompositionMatrix"),
  definition = function(object, ordering = seq_len(ncol(object)), weights = FALSE) {
    D <- ncol(object)
    parts <- colnames(object)

    ## Reorder
    if (length(weights) == D) weights <- weights[ordering]
    parts <- parts[ordering]
    object <- object[, ordering]

    plr <- matrix(0, nrow = nrow(object), ncol = D - 1)
    z <- character(D - 1)
    w <- numeric(D - 1)
    for (j in seq_len(D - 1)) {
      ilr <- ilr_single(object, n = j, d = (j + 1):D, weights = weights)
      plr[, j] <- ilr$ilr
      z[j] <- ilr$ratio
      w[j] <- ilr$weights
    }

    colnames(plr) <- paste0("Z", seq_len(D - 1))
    rownames(plr) <- rownames(object)
    .PLR(plr, parts = parts, ratio = z, weights = w, order = ordering)
  }
)

# Backtransform ================================================================
## CLR -------------------------------------------------------------------------
#' @export
#' @rdname inverse-log-ratio
#' @aliases transform_inverse,CLR-method
setMethod(
  f = "transform_inverse",
  signature = signature(object = "CLR"),
  definition = function(object) {
    x <- exp(object) / rowSums(exp(object))
    as_composition(x)
  }
)
## ALR -------------------------------------------------------------------------
#' @export
#' @rdname inverse-log-ratio
#' @aliases transform_inverse,ALR-method
setMethod(
  f = "transform_inverse",
  signature = signature(object = "ALR"),
  definition = function(object) {
    j <- object@denominator
    x <- matrix(data = 0, nrow = nrow(object), ncol = ncol(object) + 1)
    dimnames(x) <- list(rownames(object), object@parts)

    parts <- exp(object) / (1 + rowSums(exp(object)))
    x[, j] <- 1 - rowSums(parts)
    x[, -j] <- parts
    as_composition(x)
  }
)
## ILR -------------------------------------------------------------------------
#' @export
#' @rdname inverse-log-ratio
#' @aliases transform_inverse,ILR-method
setMethod(
  f = "transform_inverse",
  signature = signature(object = "ILR"),
  definition = function(object) {

    y <- tcrossprod(object, object@base)
    y <- exp(y)

    dimnames(y) <- list(rownames(object), object@parts)
    as_composition(y)
  }
)
## PLR -------------------------------------------------------------------------
#' @export
#' @rdname inverse-log-ratio
#' @aliases transform_inverse,PLR-method
setMethod(
  f = "transform_inverse",
  signature = signature(object = "PLR"),
  definition = function(object) {
    D <- ncol(object) + 1
    seq_parts <- seq_len(D)
    x <- -object

    y <- matrix(data = 0, nrow = nrow(object), ncol = D)
    dimnames(y) <- list(rownames(object), object@parts)

    norm <- !all(object@norm == 1)
    p <- object@pivot
    y[, p] <- if (norm) -sqrt((D - 1) / D) * x[, 1] else x[, 1]

    for (i in seq_parts[-p]) {
      for (j in seq_len(i - 1)) {
        y[, i] <- y[, i] + x[, j] / if (norm) sqrt((D - j + 1) * (D - j)) else 1
      }
    }
    for (i in 2:(ncol(y) - 1)) {
      y[, i] <- y[, i] - x[, i] * if (norm) sqrt((D - i) / (D - i + 1)) else 1
    }
    z <- exp(y)
    as_composition(z)
  }
)
