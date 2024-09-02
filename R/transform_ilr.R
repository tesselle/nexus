# DATA TRANSFORMATION: ISOMETRIC LOG RATIO
#' @include AllGenerics.R
NULL

# ILR ==========================================================================
ilr_base <- function(D, method = "basic") {
  ## Validation
  method <- match.arg(method, several.ok = FALSE)

  seq_parts <- seq_len(D - 1)

  ## Original ILR transformation defined by Egozcue et al. 2003
  if (method == "basic") {
    ## Helmert matrix (rotation matrix)
    H <- stats::contr.helmert(D)                  # D x D-1
    H <- t(H) / sqrt((seq_parts + 1) * seq_parts) # D-1 x D

    ## Center
    M <- diag(x = 1, nrow = D) - matrix(data = 1 / D, nrow = D, ncol = D)
    V <- tcrossprod(M, H)
  }

  V
}

#' @export
#' @rdname transform_ilr
#' @aliases transform_ilr,CompositionMatrix,missing-method
setMethod(
  f = "transform_ilr",
  signature = c(object = "CompositionMatrix"),
  definition = function(object) {
    weights <- rep(1 / ncol(object), ncol(object))
    base <- ilr_base(D = ncol(object), method = "basic")
    .transform_ilr(object, base, weights)
  }
)

#' @export
#' @rdname transform_ilr
#' @aliases transform_ilr,CLR,missing-method
setMethod(
  f = "transform_ilr",
  signature = c(object = "CLR"),
  definition = function(object) {
    weights <- object@weights
    base <- ilr_base(D = ncol(object), method = "basic")
    object@.Data <- exp(object@.Data)
    .transform_ilr(object, base, weights)
  }
)

#' @export
#' @rdname transform_ilr
#' @aliases transform_ilr,ALR,missing-method
setMethod(
  f = "transform_ilr",
  signature = c(object = "ALR"),
  definition = function(object) {
    object <- transform_clr(object)
    methods::callGeneric(object)
  }
)

.transform_ilr <- function(object, base, weights) {
  D <- ncol(object)
  seq_parts <- seq_len(D - 1)
  parts <- colnames(object)

  ## Rotated and centered values
  y <- log(object, base = exp(1))
  ilr <- y %*% base

  ratio <- vapply(
    X = seq_parts,
    FUN = function(i, k) {
      sprintf("(%s)/%s", paste0(k[seq_len(i)], collapse = ","), k[i + 1])
    },
    FUN.VALUE = character(1),
    k = parts
  )
  colnames(ilr) <- paste0("Z", seq_parts)
  rownames(ilr) <- rownames(object)

  .ILR(
    ilr,
    parts = parts,
    ratio = ratio,
    order = seq_len(D),
    base = base,
    weights = weights,
    totals = totals(object),
    groups = groups(object)
  )
}

# Univariate ILR ===============================================================
#' @export
#' @rdname univariate_ilr
#' @aliases univariate_ilr,numeric-method
setMethod(
  f = "univariate_ilr",
  signature = c(object = "numeric"),
  definition = function(object) {
    sqrt(1 / 2) * log(object / (1 - object))
  }
)

#' @export
#' @rdname univariate_ilr
#' @aliases univariate_ilr,matrix-method
setMethod(
  f = "univariate_ilr",
  signature = c(object = "matrix"),
  definition = function(object) {
    apply(X = object, MARGIN = 2, FUN = univariate_ilr)
  }
)
