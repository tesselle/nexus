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
    if (!any_replicated(x)) {
      warning("No observations are repeated.", call. = FALSE)
    }
    index <- get_samples(x)
  }
  if (by == "groups") {
    if (!any_assigned(x)) {
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
    ...,
    simplify = FALSE
  )
  do.call(rbind, m)
}

#' @export
#' @rdname aggregate
#' @aliases aggregate,CompositionMatrix-method
setMethod("aggregate", "CompositionMatrix", aggregate.CompositionMatrix)

# Margin =======================================================================
#' @export
#' @rdname margin
#' @aliases margin,CompositionMatrix-method
setMethod(
  f = "margin",
  signature = c("CompositionMatrix"),
  definition = function(x, parts = c(1, 2), name = "*") {
    ## Validation
    p <- NCOL(x)
    parts <- unique(parts)
    if (is.character(parts)) parts <- match(parts, colnames(x))
    if (p <= length(parts)) return(x)
    if (p == length(parts) + 1) {
      d <- seq_len(p)
      d <- c(d[parts], d[-parts])
      return(x[, d, drop = FALSE])
    }

    rest <- x[, -parts, drop = FALSE]
    star <- apply(X = rest, MARGIN = 1, FUN = gmean)
    mar <- cbind(x[, parts, drop = FALSE], star)
    colnames(mar) <- c(colnames(x)[parts], name[[1L]])

    clo <- closure(mar)
    methods::initialize(x, clo)
  }
)

# Mean =========================================================================
#' @export
#' @method mean CompositionMatrix
mean.CompositionMatrix <- function(x, ..., na.rm = FALSE) {
  m <- apply(X = x, MARGIN = 2, FUN = gmean, na.rm = na.rm, simplify = TRUE)
  closure(m)
}

#' @export
#' @rdname mean
#' @aliases mean,CompositionMatrix-method
setMethod("mean", "CompositionMatrix", mean.CompositionMatrix)

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

# Scale ========================================================================
#' @export
#' @method scale CompositionMatrix
scale.CompositionMatrix <- function(x, center = TRUE, scale = TRUE) {
  if (isFALSE(center) & isFALSE(scale)) return(x)

  y <- x
  if (!isFALSE(center)) {
    if (isTRUE(center)) center <- mean(x)
    arkhe::assert_type(center, "numeric")
    arkhe::assert_length(center, NCOL(x))

    y <- perturbation(y, 1 / center)
  }

  if (!isFALSE(scale)) {
    if (isTRUE(scale)) scale <- sqrt(mean(diag(covariance(x, center = TRUE))))
    arkhe::assert_type(scale, "numeric")

    y <- powering(y, 1 / scale)
  }

  y
}

#' @export
#' @rdname scale
#' @aliases scale,CompositionMatrix-method
setMethod("scale", "CompositionMatrix", scale.CompositionMatrix)

# Metric variance ==============================================================
#' @export
#' @rdname metric_var
#' @aliases metric_var,CompositionMatrix-method
setMethod(
  f = "metric_var",
  signature = c("CompositionMatrix"),
  definition = function(x) {
    sum(diag(covariance(x, center = TRUE)))
  }
)

# Metric standard deviation ====================================================
#' @export
#' @rdname metric_var
#' @aliases metric_sd,CompositionMatrix-method
setMethod(
  f = "metric_sd",
  signature = c("CompositionMatrix"),
  definition = function(x) {
    sqrt((1 / (ncol(x) - 1)) * metric_var(x))
  }
)

# Variance =====================================================================
#' @export
#' @rdname variation
#' @aliases variation,CompositionMatrix-method
setMethod(
  f = "variation",
  signature = c(x = "CompositionMatrix"),
  definition = function(x) {
    J <- ncol(x)
    parts <- colnames(x)

    varia <- utils::combn(
      x = seq_len(J),
      m = 2,
      FUN = function(i, coda) {
        z <- log(coda[, i[1]] / coda[, i[2]], base = exp(1))
        stats::var(z)
      },
      coda = x
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
#' @aliases covariance,CompositionMatrix-method
setMethod(
  f = "covariance",
  signature = c(x = "CompositionMatrix"),
  definition = function(x, center = TRUE, method = "pearson") {
    x <- if (center) transform_clr(x) else transform_alr(x)
    methods::callGeneric(x = x, method = method)
  }
)

#' @export
#' @describeIn covariance Computes the log-ratio covariance matrix
#'  (Aitchison 1986, definition 4.5).
#' @aliases covariance,ALR-method
setMethod(
  f = "covariance",
  signature = c(x = "ALR"),
  definition = function(x, method = "pearson") {
    stats::cov(x, method = method)
  }
)

#' @export
#' @describeIn covariance Computes the centered log-ratio covariance matrix
#'  (Aitchison 1986, definition 4.6).
#' @aliases covariance,ALR-method
setMethod(
  f = "covariance",
  signature = c(x = "CLR"),
  definition = function(x, method = "pearson") {
    stats::cov(x, method = method)
  }
)

# Variation array ==============================================================
# @export
# @rdname variation_array
# @aliases variation_array,CompositionMatrix-method
# setMethod(
#   f = "variation_array",
#   signature = c(object = "CompositionMatrix"),
#   definition = function(object) {
#     J <- ncol(object)
#     cbn <- utils::combn(seq_len(J), 2)
#     varia <- apply(
#       X = cbn,
#       MARGIN = 2,
#       FUN = function(j, x) {
#         mean(log(x[, j[1]] / x[, j[2]]))
#       },
#       x = object
#     )
#
#     mtx <- variation(object)
#     mtx[lower.tri(mtx, diag = FALSE)] <- varia
#     mtx
#   }
# )

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

# Mahalanobis ==================================================================
#' @export
#' @method mahalanobis CompositionMatrix
mahalanobis.CompositionMatrix <- function(x, center, cov, ..., robust = TRUE,
                                          method = c("mve", "mcd")) {
  ## Transformation
  x <- transform_ilr(x)
  mahalanobis(x, center, cov, ..., robust = robust, method = method)
}

#' @export
#' @rdname mahalanobis
#' @aliases mahalanobis,CompositionMatrix-method
setMethod("mahalanobis", "CompositionMatrix", mahalanobis.CompositionMatrix)

#' @export
#' @method mahalanobis ILR
mahalanobis.ILR <- function(x, center, cov, ..., robust = TRUE,
                            method = c("mve", "mcd")) {

  if (missingORnull(center) | missingORnull(cov)) {
    if (!robust) method <- "classical" # Standard estimators
    else method <- match.arg(method, several.ok = FALSE) # Robust estimators
    v <- MASS::cov.rob(x, method = method, ...)
  }

  est <- list(center = NULL, cov = NULL)
  est$center <- if (missingORnull(center)) v$center else center
  est$cov <- if (missingORnull(cov)) v$cov else cov

  message(v$sing)

  stats::mahalanobis(x, center = est$center, cov = est$cov)
}

#' @export
#' @rdname mahalanobis
#' @aliases mahalanobis,ILR-method
setMethod("mahalanobis", "ILR", mahalanobis.ILR)
