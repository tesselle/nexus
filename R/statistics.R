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
  x <- transform_clr(x)
  stats::dist(x, method = method, diag = diag, upper = upper, p = p)
}

#' @export
#' @rdname dist
#' @aliases dist,CompositionMatrix-method
setMethod("dist", "CompositionMatrix", dist.CompositionMatrix)


#' @export
#' @method mahalanobis CompositionMatrix
mahalanobis.CompositionMatrix <- function(x, center, cov, robust = TRUE, ...) {
  ## Transformation
  x <- transform_ilr(x)

  if (missingORnull(center) | missingORnull(cov)) {
    if (robust) {
      ## Robust estimators
      v <- robustbase::covMcd(x, ...)
    } else {
      ## Standard estimators
      v <- list(mean = colMeans(x), cov = stats::cov(x))
    }

    est <- list(center = NULL, cov = NULL)
    est$center <- if (missingORnull(center)) v$center else center
    est$cov <- if (missingORnull(cov)) v$cov else cov
  }

  stats::mahalanobis(x, center = est$center, cov = est$cov)
}

#' @export
#' @rdname mahalanobis
#' @aliases mahalanobis,CompositionMatrix-method
setMethod("mahalanobis", "CompositionMatrix", mahalanobis.CompositionMatrix)

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
