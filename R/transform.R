# DATA TRANSFORMATION
#' @include AllClasses.R AllGenerics.R
NULL

# CLR ==========================================================================
#' @export
#' @rdname transform
#' @aliases transform_clr,CompositionMatrix-method
setMethod(
  f = "transform_clr",
  signature = signature(object = "CompositionMatrix"),
  definition = function(object, base = exp(1)) {
    parts <- colnames(object)
    gmean <- apply(X = object, MARGIN = 1, FUN = gmean)

    clr <- log(object / gmean, base = base)
    dimnames(clr) <- dimnames(object)

    values <- as.numeric(clr)
    .CLR(clr, parts = parts, base = base, mean = gmean)
  }
)

# ALR ==========================================================================
#' @export
#' @rdname transform
#' @aliases transform_alr,CompositionMatrix-method
setMethod(
  f = "transform_alr",
  signature = signature(object = "CompositionMatrix"),
  definition = function(object, j = ncol(object), base = exp(1)) {
    parts <- colnames(object)
    j <- if (is.character(j)) which(parts == j) else as.integer(j)
    d <- object[, j, drop = TRUE]

    alr <- log(object[, -j, drop = FALSE] / d, base = base)
    dimnames(alr) <- list(rownames(object),
                          paste(parts[-j], parts[j], sep = "_"))

    .ALR(alr, parts = parts, base = base,
         rationing_values = d, rationing_index = j)
  }
)

# ILR ==========================================================================
#' @export
#' @rdname transform
#' @aliases transform_ilr,CompositionMatrix-method
setMethod(
  f = "transform_ilr",
  signature = signature(object = "CompositionMatrix"),
  definition = function(object, base = exp(1)) {
    D <- ncol(object)
    parts <- colnames(object)

    ilr <- matrix(data = NA, nrow = nrow(object), ncol = D - 1)
    z <- vector(mode = "character", length = D - 1)
    for (i in seq_len(D - 1)) {
      norm <- sqrt(i / (i + 1))
      k <- apply(X = object[, 1:i, drop = FALSE], MARGIN =  1, FUN = gmean)
      ilr[, i] <- norm * log(k / (object[, (i + 1)]), base = base)
      z[i] <- paste(paste0(parts[1:i], collapse = "-"), parts[i + 1],
                    sep = "_")
    }

    colnames(ilr) <- paste0("Z", seq_len(D - 1))
    rownames(ilr) <- rownames(object)
    .ILR(ilr, parts = parts, base = base, ratio = z)
  }
)

# Pivot ========================================================================
#' @export
#' @rdname transform
#' @aliases transform_pivot,CompositionMatrix-method
setMethod(
  f = "transform_pivot",
  signature = signature(object = "CompositionMatrix"),
  definition = function(object, pivot = 1, base = exp(1)) {
    D <- ncol(object)
    parts <- colnames(object)

    # Reorder
    p <- if (is.character(pivot)) which(parts == pivot) else as.integer(pivot)
    w <- c(p, seq_along(parts)[-p])

    parts <- parts[w]
    object <- object[, w]

    ilr <- matrix(data = NA, nrow = nrow(object), ncol = D - 1)
    z <- vector(mode = "character", length = D - 1)
    for (i in seq_len(D - 1)) {
      norm <- sqrt((D - i) / (D - i + 1))
      k <- apply(X = object[, (i + 1):D, drop = FALSE], MARGIN =  1, FUN = gmean)
      ilr[, i] <- norm * log(k / (object[, i]), base = base)
      z[i] <- paste(parts[i], paste0(parts[(i + 1):D], collapse = "-"),
                    sep = "_")
    }

    colnames(ilr) <- paste0("Z", seq_len(D - 1))
    rownames(ilr) <- rownames(object)
    .ILR(-ilr, parts = parts, base = base, pivot = p, ratio = z, norm = norm)
  }
)
