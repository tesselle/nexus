# BARPLOT
#' @include AllGenerics.R
NULL

# CompositionMatrix ============================================================
#' @export
#' @method barplot CompositionMatrix
barplot.CompositionMatrix <- function(height, ..., groups = get_groups(height),
                                      order = NULL, decreasing = FALSE,
                                      horiz = TRUE,
                                      xlab = NULL, ylab = NULL,
                                      main = NULL, sub = NULL,
                                      ann = graphics::par("ann"), axes = TRUE,
                                      col = grDevices::hcl.colors(ncol(height), "viridis"),
                                      legend = list(x = "top")) {
  ## Get data
  z <- height

  ## Ordering
  ordering <- seq_len(nrow(height))
  if (!is.null(order)) {
    i <- z[, order, drop = TRUE]
    ordering <- order(i, decreasing = decreasing)
  }
  z <- z[ordering, , drop = FALSE]

  ## Graphical parameters
  cex.lab <- list(...)$cex.lab %||% graphics::par("cex.lab")
  col.lab <- list(...)$col.lab %||% graphics::par("col.lab")
  font.lab <- list(...)$font.lab %||% graphics::par("font.lab")
  cex.main <- list(...)$cex.main %||% graphics::par("cex.main")
  font.main <- list(...)$font.main %||% graphics::par("font.main")
  col.main <- list(...)$col.main %||% graphics::par("col.main")

  x_side <- if (horiz) 1 else 2
  y_side <- if (horiz) 2 else 1

  ## Grouping
  if (has_groups(groups)) {
    arkhe::assert_length(groups, nrow(z))
    groups <- groups[ordering]

    z <- split(z, f = groups)
    n <- length(z)

    ylabs <- ylab %||% names(z) %||% paste0("G", seq_len(n))

    ## Save and restore
    old_par <- graphics::par(
      mar = if (horiz) c(0, 5.1, 0, 1) else  c(5.1, 0, 0, 1),
      oma = if (horiz) c(6, 0, 5, 0) else c(0, 6, 5, 0),
      mfcol = if (horiz) c(n, 1) else c(1, n)
    )
    on.exit(graphics::par(old_par))

    for (i in seq_len(n)) {
      graphics::barplot(height = t(z[[i]]), horiz = horiz, axes = FALSE,
                        main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
                        col = col, las = x_side, ...)

      ## Construct Axis
      do_x <- (horiz & i == n) | (!horiz & i == 1)
      if (axes) {
        if (do_x) {
          at <- graphics::axTicks(side = x_side)
          graphics::axis(side = x_side, at = at, labels = label_percent(at),
                         xpd = NA, las = 1)
        }
      }

      ## Add annotation
      if (ann) {
        if (do_x) {
          graphics::mtext(xlab, side = x_side, line = 3, cex = cex.lab,
                          col = col.lab, font = font.lab)
        }
        graphics::mtext(ylabs[i], side = y_side, line = 3, cex = cex.lab,
                        col = col.lab, font = font.lab)
      }
    }

    ## Add annotation
    if (ann) {
      graphics::par(mfcol = c(1, 1))
      graphics::mtext(main, side = 3, line = 3, cex = cex.main,
                      font = font.main, col = col.main)
    }
  } else {
    graphics::barplot(height = t(z), horiz = horiz, col = col, las = 1,
                      main = main, sub = sub, xlab = xlab, ylab = ylab,
                      axes = FALSE, ann = ann, ...)
    at <- graphics::axTicks(side = x_side)
    graphics::axis(side = x_side, at = at, labels = label_percent(at),
                   xpd = NA, las = 1)
  }

  ## Add legend
  # https://stackoverflow.com/a/42076830
  if (is.list(legend)) {
    leg_par <- graphics::par(mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0),
                             mfcol = c(1, 1), new = TRUE)
    on.exit(graphics::par(leg_par), add = TRUE)
    graphics::plot(0, 0, type = "n", ann = FALSE, axes = FALSE)

    ## Compute legend position
    args <- list(x = "top", legend = colnames(height), fill = col,
                 ncol = ceiling(ncol(height) / 2), bty = "n", xpd = NA)
    args <- utils::modifyList(args, legend)

    ## Plot legend
    do.call(graphics::legend, args = args)
  }

  invisible(height)
}

#' @export
#' @rdname barplot
#' @aliases barplot,CompositionMatrix-method
setMethod("barplot", c(height = "CompositionMatrix"), barplot.CompositionMatrix)
