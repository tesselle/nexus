# MARGIN
#' @include AllGenerics.R
NULL

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
