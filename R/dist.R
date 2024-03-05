# DISTANCES
#' @include AllGenerics.R
NULL

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
