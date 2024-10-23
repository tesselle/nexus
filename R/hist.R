# HISTOGRAM
#' @include AllGenerics.R
NULL

# CompositionMatrix ============================================================
#' @export
#' @method hist CompositionMatrix
hist.CompositionMatrix <- function(x, ..., select = 1,
                                   breaks = "Sturges",
                                   freq = FALSE, labels = FALSE,
                                   main = NULL, sub = NULL,
                                   ann = graphics::par("ann"),
                                   axes = TRUE, frame.plot = axes) {
  ## Validation
  if (is.character(select)) select <- match(select, labels(x))
  arkhe::assert_length(select, 1)

  ## Graphical parameters
  cex.lab <- list(...)$cex.lab %||% graphics::par("cex.lab")
  col.lab <- list(...)$col.lab %||% graphics::par("col.lab")
  font.lab <- list(...)$font.lab %||% graphics::par("font.lab")
  cex.main <- list(...)$cex.main %||% graphics::par("cex.main")
  col.main <- list(...)$col.main %||% graphics::par("col.main")
  font.main <- list(...)$font.main %||% graphics::par("font.main")

  ## Compute univariate ilr transformation
  z <- univariate_ilr(x)

  ## Select one compositonal part
  xi <- x[, select, drop = TRUE]
  zi <- z[, select, drop = TRUE]

  ## Compute axis in percent
  lab_i <- pretty(xi, n = 6)
  lab_i <- lab_i[lab_i > 0]
  at_i <- univariate_ilr(lab_i)

  ## Plot histogram
  h <- graphics::hist(x = zi, breaks = breaks, plot = FALSE)
  xlim <- range(at_i, h$breaks, finite = TRUE)
  plot(h, freq = freq, xlim = xlim, labels = labels, ...,
       main = main, sub = sub, xlab = NULL, ylab = NULL, axes = FALSE)

  ## Construct axis
  if (axes) {
    graphics::axis(side = 1, xpd = NA, las = 1)
    graphics::axis(side = 3, at = at_i, labels = label_percent(lab_i),
                   xpd = NA, las = 1)
    graphics::axis(side = 2, xpd = NA, las = 1)
  }

  ## Plot frame
  if (frame.plot) {
    graphics::box()
  }

  ## Add annotation
  if (ann) {
    xlab <- labels(x)[select]
    ylab <- "Frequency"
    graphics::mtext(sprintf("ilr(%s)", xlab), side = 1, line = 3,
                    cex = cex.lab, col = col.lab, font = font.lab)
    graphics::mtext(sprintf("%s %%", xlab), side = 3, line = 3,
                    cex = cex.lab, col = col.lab, font = font.lab)
    graphics::mtext(ylab, side = 2, line = 3,
                    cex = cex.lab, col = col.lab, font = font.lab)
  }

  invisible(x)
}

#' @export
#' @rdname hist
#' @aliases hist,CompositionMatrix-method
setMethod("hist", c(x = "CompositionMatrix"), hist.CompositionMatrix)
