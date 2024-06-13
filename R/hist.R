# HISTOGRAM
#' @include AllGenerics.R
NULL

# CompositionMatrix ============================================================
#' @export
#' @method hist CompositionMatrix
hist.CompositionMatrix <- function(x, ..., freq = FALSE,
                                   ncol = NULL, flip = FALSE,
                                   main = NULL, sub = NULL,
                                   ann = graphics::par("ann"),
                                   axes = TRUE, frame.plot = axes) {
  m <- nrow(x)
  p <- ncol(x)

  ## Plot
  if (is.null(ncol)) ncol <- if (p > 4) 2 else 1
  nrow <- ceiling(p / ncol)

  ## Graphical parameters
  ## Save and restore
  if (p > 1) {
    old_par <- graphics::par(
      mar = c(4.1, 5.1, 4.1, if (flip) 5.1 else 2.1),
      oma = c(0, 0, 5, 0),
      mfcol = c(nrow, ncol)
    )
    on.exit(graphics::par(old_par))
  }
  cex.lab <- list(...)$cex.lab %||% graphics::par("cex.lab")
  if (p > 1) cex.lab <- cex.lab * ifelse(max(m, p) < 3, 0.83,  0.66) # See ?par
  col.lab <- list(...)$col.lab %||% graphics::par("col.lab")
  font.lab <- list(...)$font.lab %||% graphics::par("font.lab")
  cex.main <- list(...)$cex.main %||% graphics::par("cex.main")
  col.main <- list(...)$col.main %||% graphics::par("col.main")
  font.main <- list(...)$font.main %||% graphics::par("font.main")

  ## Compute univariate ilr transformation
  z <- univariate_ilr(x)

  index <- seq_len(p)
  for (j in index) {
    xi <- x[, j, drop = TRUE]
    zi <- z[, j, drop = TRUE]

    lab_i <- pretty(xi, n = 6)
    lab_i <- lab_i[lab_i > 0]
    at_i <- univariate_ilr(lab_i)

    ## Histogram
    h <- graphics::hist(x = zi, ..., plot = FALSE)
    xlim <- range(at_i, h$breaks, finite = TRUE)
    plot(h, freq = freq, xlim = xlim,
         main = NULL, sub = NULL, xlab = NULL, ylab = NULL, axes = FALSE)

    ## Construct axis
    y_side <- if (j %% 2 || !flip) 2 else 4
    if (axes) {
      graphics::axis(side = 1, xpd = NA, las = 1)
      graphics::axis(side = 3, at = at_i, labels = label_percent(lab_i),
                     xpd = NA, las = 1)
      graphics::axis(side = y_side, xpd = NA, las = 1)
    }

    ## Plot frame
    if (frame.plot) {
      graphics::box()
    }

    ## Add annotation
    if (ann) {
      xlab <- colnames(x)[j]
      ylab <- "Frequency"
      graphics::mtext(sprintf("ilr(%s)", xlab), side = 1, line = 2.5,
                      cex = cex.lab, col = col.lab, font = font.lab)
      graphics::mtext(sprintf("%s %%", xlab), side = 3, line = 2.5,
                      cex = cex.lab, col = col.lab, font = font.lab)
      graphics::mtext(ylab, side = y_side, line = 3,
                      cex = cex.lab, col = col.lab, font = font.lab)
    }
  }

  ## Add annotation
  if (ann) {
    graphics::par(mfcol = c(1, 1))
    graphics::mtext(main, side = 3, line = 3,
                    cex = cex.main, col = col.main, font = font.main)
  }

  invisible(x)
}

#' @export
#' @rdname hist
#' @aliases hist,CompositionMatrix-method
setMethod("hist", c(x = "CompositionMatrix"), hist.CompositionMatrix)
