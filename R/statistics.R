# STATISTICS
#' @include AllGenerics.R AllClasses.R
NULL

# Variance =====================================================================
#' @export
#' @method var CompositionMatrix
var.CompositionMatrix <- function(x) {
  J <- ncol(x)
  parts <- colnames(x)

  cbn <- utils::combn(seq_len(J), 2)
  varia <- apply(
    X = cbn,
    MARGIN = 2,
    FUN = function(j, x) {
      stats::var(log(x[, j[1]] / x[, j[2]], base = exp(1)))
    },
    x = x
  )

  mtx <- matrix(data = 0, nrow = J, ncol = J)
  mtx[lower.tri(mtx, diag = FALSE)] <- varia
  mtx <- t(mtx)
  mtx[lower.tri(mtx, diag = FALSE)] <- varia

  dimnames(mtx) <- list(parts, parts)
  mtx
}

#' @export
#' @rdname covariance
#' @aliases var,CompositionMatrix-method
setMethod("var", "CompositionMatrix", var.CompositionMatrix)

# Covariance ===================================================================
#' @export
#' @method cov CompositionMatrix
cov.CompositionMatrix <- function(x) {
  stats::cov(transform_lr(x))
}

#' @export
#' @rdname covariance
#' @aliases cov,CompositionMatrix-method
setMethod("cov", "CompositionMatrix", cov.CompositionMatrix)

# Variation ====================================================================
#' @export
#' @rdname variation
#' @aliases variation,CompositionMatrix-method
setMethod(
  f = "variation",
  signature = signature(object = "CompositionMatrix"),
  definition = function(object) {
    J <- ncol(object)
    cbn <- utils::combn(seq_len(J), 2)
    varia <- apply(
      X = cbn,
      MARGIN = 2,
      FUN = function(j, x) {
        mean(log(x[, j[1]] / x[, j[2]]))
      },
      x = object
    )

    mtx <- var(object)
    mtx[lower.tri(mtx, diag = FALSE)] <- varia
    mtx
  }
)

# Distances ====================================================================
#' @export
#' @method dist CompositionMatrix
dist.CompositionMatrix <- function(x, method = "euclidean",
                                   diag = FALSE, upper = FALSE, p = 2) {
  stats::dist(transform_clr(x), method = method, diag = diag, upper = upper,
              p = p)
}

#' @export
#' @rdname distance
#' @aliases dist,CompositionMatrix-method
setMethod("dist", "CompositionMatrix", dist.CompositionMatrix)

#' Mahalanobis Distance
#'
#' @param object A numeric matrix.
#' @param group A numeric matrix.
#' @param robust A [`logical`] scalar.
#' @param alpha A length-one [`numeric`] vector controlling the size of the
#'  subsets over which the determinant is minimized (see
#'  [robustbase::covMcd()]). Only used if `robust` is `TRUE`.
#' @param ... Extra parameter to be passed to [transform_pivot()].
#' @return
#'  A [`numeric`] vector giving the squared Mahalanobis distance of all rows in
#'  `object`.
#' @seealso [stats::mahalanobis()]
#' @keywords internal
#' @noRd
stats_mahalanobis <- function(object, group, robust = TRUE, alpha = 0.5, ...) {
  if (missing(group)) group <- object

  ilr_object <- transform_plr(object, ...)
  ilr_group <- transform_plr(group, ...)

  x <- as.matrix(ilr_object)
  y <- as.matrix(ilr_group)
  if (robust) {
    # Robust estimators
    v <- robustbase::covMcd(y, alpha = alpha)
    est <- list(mean = v$center, cov = v$cov)
  } else {
    # Standard estimators
    est <- list(mean = colMeans(y, na.rm = TRUE), cov = stats::cov(y))
  }

  stats::mahalanobis(x, center = est$mean, cov = est$cov)
}

# Tools ========================================================================
#' Geometric Mean
#'
#' @param x A [`numeric`] vector.
#' @return A [`numeric`] vector.
#' @keywords internal
#' @noRd
gmean <- function(x) {
  index <- is.finite(x) & x > 0
  exp(mean(log(unclass(x)[index])))
}
