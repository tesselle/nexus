# VARIATION MATRIX
#' @include AllGenerics.R
NULL

# Variation matrix =============================================================
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

#' @export
#' @rdname pip
#' @aliases pip,CompositionMatrix-method
setMethod(
  f = "pip",
  signature = c(x = "CompositionMatrix"),
  definition = function(x) {
    v <- variation(x)
    1 / (1 + sqrt(v))
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
