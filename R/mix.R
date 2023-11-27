# MIXED-MODE ANALYSIS
#' @include AllGenerics.R
NULL

# First approach ===============================================================
#' @export
#' @describeIn mix First approach of mixed-mode analysis.
#' @aliases mix,matrix,matrix-method
setMethod(
  f = "mix",
  signature = c(x = "matrix", y = "matrix"),
  definition = function(x, y, lambda = 1, ...) {
    ## Validation
    arkhe::needs("cluster")
    stopifnot(nrow(y) == nrow(x))

    X <- vector(mode = "list", length = lambda + 1)
    X[[1]] <- y
    lambda <- as.integer(lambda[[1L]])
    for (i in seq_len(lambda)) X[[i + 1]] <- x
    X <- do.call(cbind, X)

    d <- cluster::daisy(X, metric = "gower", ...)
    as.dist(d)
  }
)

# Second approach ==============================================================
#' @export
#' @describeIn mix Second approach of mixed-mode analysis.
#' @aliases mix,dist,dist-method
setMethod(
  f = "mix",
  signature = c(x = "dist", y = "dist"),
  definition = function(x, y, mu = 0.5) {
    ## Validation
    stopifnot(mu >= 0 & mu <= 1)

    x <- as.matrix(x)
    y <- as.matrix(y)

    d <- mu * x + (1 - mu) * y
    as.dist(d)
  }
)
