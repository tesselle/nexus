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
                                    palette_color = palette_color_discrete(),
                                    palette_symbol = palette_shape()) {
  ## Aesthetics
  lvl <- group_names(x)
  col <- palette_color(lvl)
  pch <- palette_symbol(lvl)

  isopleuros::ternary_pairs(x, margin = margin, col = col, pch = pch, ...)
  invisible(x)
}

#' @export
#' @rdname pairs
#' @aliases pairs,GroupedComposition-method
setMethod("pairs", c(x = "GroupedComposition"), pairs.GroupedComposition)
