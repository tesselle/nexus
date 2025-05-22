# PAIRS
#' @include AllGenerics.R
NULL

# CompositionMatrix ============================================================
#' @export
#' @method pairs CompositionMatrix
pairs.CompositionMatrix <- function(x, margin = NULL, ...) {
  isopleuros::ternary_pairs(x, margin = margin, ...)
  invisible(x)
}

#' @export
#' @rdname pairs
#' @aliases pairs,CompositionMatrix-method
setMethod("pairs", c(x = "CompositionMatrix"), pairs.CompositionMatrix)

#' @export
#' @method pairs GroupedComposition
pairs.GroupedComposition <- function(x, ..., margin = NULL,
                                     color = NULL, symbol = NULL) {
  ## Aesthetics
  lvl <- group_names(x)
  col <- khroma::palette_color_discrete(color)(lvl)
  pch <- khroma::palette_shape(symbol)(lvl)

  isopleuros::ternary_pairs(x, margin = margin, col = col, pch = pch, ...)
  invisible(x)
}

#' @export
#' @rdname pairs
#' @aliases pairs,GroupedComposition-method
setMethod("pairs", c(x = "GroupedComposition"), pairs.GroupedComposition)
