# STATISTICS
#' @include AllGenerics.R
NULL

# Aggregate ====================================================================
#' @export
#' @method aggregate CompositionMatrix
aggregate.CompositionMatrix <- function(x, by, FUN, ...) {
  ## Validation
  by <- match.arg(by, choices = c("samples", "groups"), several.ok = FALSE)

  if (by == "samples") {
    if (!has_replicates(x)) {
      warning("No observations are repeated.", call. = FALSE)
    }
    index <- get_samples(x)
  }
  if (by == "groups") {
    if (!has_groups(x)) {
      stop("No group is defined.", call. = FALSE)
    }
    index <- get_groups(x)
  }

  m <- tapply(
    X = seq_len(nrow(x)),
    INDEX = index,
    FUN = function(i, data, fun, ...) fun(data[i, , drop = FALSE], ...),
    data = x,
    fun = FUN,
    ...
  )
  do.call(rbind, m)
}

#' @export
#' @rdname aggregate
#' @aliases aggregate,CompositionMatrix-method
setMethod("aggregate", "CompositionMatrix", aggregate.CompositionMatrix)

# Mean =========================================================================
#' @export
#' @method mean CompositionMatrix
mean.CompositionMatrix <- function(x, ...) {
  m <- apply(X = x, MARGIN = 2, FUN = gmean, ..., simplify = TRUE)
  closure(m)
}

#' @export
#' @rdname mean
#' @aliases mean,CompositionMatrix-method
setMethod("mean", "CompositionMatrix", mean.CompositionMatrix)

# Variance =====================================================================
#' @export
#' @rdname covariance
#' @aliases variance,CompositionMatrix-method
setMethod(
  f = "variance",
  signature = c(x = "CompositionMatrix"),
  definition = function(x) {
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
)

# Covariance ===================================================================
#' @export
#' @rdname covariance
#' @aliases cov,CompositionMatrix-method
setMethod(
  f = "covariance",
  signature = c(x = "CompositionMatrix"),
  definition = function(x, method = "pearson") {
    stats::cov(transform_lr(x), method = method)
  }
)

# Variation ====================================================================
#' @export
#' @rdname variation
#' @aliases variation,CompositionMatrix-method
setMethod(
  f = "variation",
  signature = c(object = "CompositionMatrix"),
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

    mtx <- variance(object)
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
mahalanobis.CompositionMatrix <- function(x, center, cov, ..., robust = TRUE) {
  ## Transformation
  x <- transform_ilr(x)

  if (missingORnull(center) | missingORnull(cov)) {
    if (robust) {
      ## Robust estimators
      v <- robustbase::covMcd(x, ...)
    } else {
      ## Standard estimators
      v <- list(center = colMeans(x), cov = stats::cov(x))
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
#' @param trim A length-one [`numeric`] vector specifying the fraction (0 to 0.5)
#'  of observations to be trimmed from each end of `x` before the mean is
#'  computed.
#' @param na.rm A [`logical`] scalar: should `NA` values be stripped before the
#'  computation proceeds?
#' @return A [`numeric`] vector.
#' @keywords internal
#' @noRd
gmean <- function(x, trim = 0, na.rm = FALSE) {
  index <- is.finite(x) & x > 0
  exp(mean(log(unclass(x)[index]), trim = trim, na.rm = na.rm))
}
