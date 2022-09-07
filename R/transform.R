# DATA TRANSFORMATION
#' @include AllClasses.R AllGenerics.R
NULL

# LR ===========================================================================
#' @export
#' @rdname transform_lr
#' @aliases transform_lr,CompositionMatrix-method
setMethod(
  f = "transform_lr",
  signature = signature(object = "CompositionMatrix"),
  definition = function(object) {
    J <- ncol(object)
    parts <- colnames(object)
    weights <- rep(1 / J, J)

    w <- unlist(utils::combn(weights, 2, FUN = function(x) Reduce(`*`, x),
                             simplify = FALSE))
    r <- unlist(utils::combn(parts, 2, FUN = paste, collapse = "_",
                             simplify = FALSE))

    jj <- utils::combn(seq_len(J), 2, simplify = FALSE)
    lr <- matrix(data = 0, nrow = nrow(object), ncol = length(jj))
    for (i in seq_along(jj)) {
      a <- jj[[i]][[1]]
      b <- jj[[i]][[2]]
      lr[, i] <- log(object[, a] / object[, b], base = exp(1))
    }

    rownames(lr) <- rownames(object)
    colnames(lr) <- r

    .LR(lr, parts = parts, ratio = r, order = seq_len(J), weights = w)
  }
)

# CLR ==========================================================================
#' @export
#' @rdname transform_clr
#' @aliases transform_clr,CompositionMatrix-method
setMethod(
  f = "transform_clr",
  signature = signature(object = "CompositionMatrix"),
  definition = function(object, weights = FALSE) {
    J <- ncol(object)
    parts <- colnames(object)

    w <- if (any(weights)) colMeans(object) else rep(1 / J, J)
    if (is.numeric(weights)) {
      arkhe::assert_length(weights, J)
      arkhe::assert_numeric(weights, "positive", strict = FALSE)
      w <- weights / sum(weights) # Sum up to 1
    }

    base <- diag(J) - matrix(data = w, nrow = J, ncol = J)
    clr <- log(object, base = exp(1)) %*% base
    dimnames(clr) <- dimnames(object)

    .CLR(clr, parts = parts, ratio = parts, order = seq_len(J),
         base = base, weights = w)
  }
)

# ALR ==========================================================================
#' @export
#' @rdname transform_alr
#' @aliases transform_alr,CompositionMatrix-method
setMethod(
  f = "transform_alr",
  signature = signature(object = "CompositionMatrix"),
  definition = function(object, j = ncol(object)) {
    D <- ncol(object)
    parts <- colnames(object)

    ## Reorder
    j <- if (is.character(j)) which(parts == j) else as.integer(j)
    ordering <- c(which(j != seq_len(D)), j)
    parts <- parts[ordering]
    object <- object[, ordering]

    base <- diag(1, nrow = D, ncol = D - 1)
    base[D, ] <- -1

    alr <- log(object, base = exp(1)) %*% base
    rownames(alr) <- rownames(object)
    colnames(alr) <- paste(parts[-D], parts[D], sep = "_")

    w <- rep(1 / D, D)
    w <- w[-D] * w[D]

    .ALR(alr, parts = parts, ratio = colnames(alr), order = order(ordering),
         base = base, weights = w)
  }
)

# ILR ==========================================================================
#' @export
#' @rdname transform_ilr
#' @aliases transform_ilr,CompositionMatrix-method
setMethod(
  f = "transform_ilr",
  signature = signature(object = "CompositionMatrix"),
  definition = function(object) {
    D <- ncol(object)
    seq_parts <- seq_len(D - 1)
    parts <- colnames(object)

    ## Helmert matrix (rotation matrix)
    H <- stats::contr.helmert(D)                  # D x D-1
    H <- t(H) / sqrt((seq_parts + 1) * seq_parts) # D-1 x D

    ## Center
    m <- diag(x = 1, nrow = D) - matrix(data = 1 / D, nrow = D, ncol = D)
    H <- tcrossprod(m, H)

    ## Rotated and centered values
    y <- log(object, base = exp(1))
    ilr <- y %*% H

    ratio <- vapply(
      X = seq_parts,
      FUN = function(i, k) {
        paste(paste0(k[seq_len(i)], collapse = "-"), k[i + 1], sep = "_")
      },
      FUN.VALUE = character(1),
      k = parts
    )
    colnames(ilr) <- paste0("Z", seq_parts)
    rownames(ilr) <- rownames(object)

    .ILR(ilr, parts = parts, ratio = ratio, order = seq_len(D),
         base = H, weights = rep(1 / D, D))
  }
)

# Pivot ========================================================================
#' @export
#' @rdname transform_plr
#' @aliases transform_plr,CompositionMatrix-method
setMethod(
  f = "transform_plr",
  signature = signature(object = "CompositionMatrix"),
  definition = function(object, pivot = 1) {
    J <- ncol(object)
    parts <- colnames(object)

    ## Reorder
    pivot <- if (is.character(pivot)) which(parts == pivot) else as.integer(pivot)
    ordering <- c(pivot, which(pivot != seq_len(J)))
    parts <- parts[ordering]
    object <- object[, ordering]

    x <- seq_len(J - 1)
    balances <- diag(sqrt((J - x) / (J - x + 1)))
    z <- 1 / matrix(data = seq_len(J) - J, nrow = J, ncol = J)
    z[lower.tri(z)] <- 0
    diag(z) <- 1
    z <- z[-nrow(z), ]

    H <- t(balances %*% z)
    plr <- log(object, base = exp(1)) %*% H

    ratio <- vapply(
      X = seq_len(J - 1),
      FUN = function(i, parts) {
        j <- length(parts)
        sprintf("%s_%s", parts[1], paste0(parts[(i+1):j], collapse = "-"))
      },
      FUN.VALUE = character(1),
      parts = parts
    )
    colnames(plr) <- paste0("Z", seq_len(J - 1))
    rownames(plr) <- rownames(object)
    .PLR(plr, parts = parts, ratio = ratio, order = order(ordering),
         base = H, weights = rep(1 / J, J))
  }
)

# Backtransform ================================================================
## CLR -------------------------------------------------------------------------
#' @export
#' @rdname transform_inverse
#' @aliases transform_inverse,CLR-method
setMethod(
  f = "transform_inverse",
  signature = signature(object = "CLR"),
  definition = function(object) {
    y <- exp(object)
    y <- y / (1 + rowSums(y))

    dimnames(y) <- list(rownames(object), object@parts)
    as_composition(y)
  }
)
## ALR -------------------------------------------------------------------------
#' @export
#' @rdname transform_inverse
#' @aliases transform_inverse,ALR-method
setMethod(
  f = "transform_inverse",
  signature = signature(object = "ALR"),
  definition = function(object) {
    y <- exp(object)
    y <- y / (1 + rowSums(y))
    z <- 1 - rowSums(y)

    y <- cbind(y, z)
    dimnames(y) <- list(rownames(object), object@parts)
    y <- y[, object@order]
    as_composition(y)
  }
)
## ILR -------------------------------------------------------------------------
#' @export
#' @rdname transform_inverse
#' @aliases transform_inverse,ILR-method
setMethod(
  f = "transform_inverse",
  signature = signature(object = "ILR"),
  definition = function(object) {
    y <- tcrossprod(object, object@base)
    y <- exp(y)

    dimnames(y) <- list(rownames(object), object@parts)
    y <- y[, object@order]
    as_composition(y)
  }
)
