# HELPERS

missingORnull <- function(x) {
  missing(x) || is.null(x)
}

#' Label Percentages
#'
#' @param x A [`numeric`] vector.
#' @param digits An [`integer`] indicating the number of decimal places.
#'  If `NULL` (the default), breaks will have the minimum number of digits
#'  needed to show the difference between adjacent values.
#' @param trim A [`logical`] scalar. If `FALSE` (the default), values are
#'  right-justified to a common width (see [base::format()]).
#' @return A [`character`] vector.
#' @keywords internal
#' @noRd
label_percent <- function(x, digits = NULL, trim = FALSE) {
  i <- !is.na(x)
  y <- x[i]
  y <- abs(y) * 100
  y <- format(y, trim = trim, digits = digits)
  y <- paste0(y, "%")
  x[i] <- y
  x
}

#' Single Panel Plot
#'
#' @param x,y A [`numeric`] vector.
#' @param name A [`character`] string.
#' @param panel A [`function`] in the form `function(x, y, name, ...)`
#'  which gives the action to be carried out in each panel of the display.
#' @param asp A length-one [`numeric`] vector giving the aspect ratio y/x
#'  (see [graphics::plot.window()]).
#' @param xlim,ylim A length-two [`numeric`] vector specifying the the x and y
#'  limits.
#' @param main A [`character`] string giving a main title for the plot.
#' @param sub A [`character`] string giving a subtitle for the plot.
#' @param ann A [`logical`] scalar: should the default annotation (title and x
#'  and y axis labels) appear on the plot?
#' @param axes A [`logical`] scalar: should axes be drawn on the plot?
#' @param frame.plot A [`logical`] scalar: should a box be drawn around the
#'  plot?
#' @param panel.first An an `expression` to be evaluated after the plot axes are
#'  set up but before any plotting takes place. This can be useful for drawing
#'  background grids.
#' @param panel.last An `expression` to be evaluated after plotting has taken
#'  place but before the axes, title and box are added.
#' @param ... Further parameters to be passed to `panel`
#'  (e.g. [graphical parameters][graphics::par]).
#' @return
#'  Called for its side-effects: it results in a graphic being displayed.
#'  Invisibly returns a list with elements `x` and `y`.
#' @keywords internal
#' @noRd
.plot_single <- function(x, y, name, panel, ..., asp = NA,
                         xlim = NULL, ylim = NULL,
                         xlab = NULL, ylab = NULL,
                         main = NULL, sub = NULL,
                         ann = graphics::par("ann"),
                         axes = TRUE, frame.plot = axes,
                         panel.first = NULL, panel.last = NULL) {
  ## Open new window
  grDevices::dev.hold()
  on.exit(grDevices::dev.flush(), add = TRUE)
  graphics::plot.new()

  ## Set plotting coordinates
  xlim <- xlim %||% range(x, finite = TRUE)
  ylim <- ylim %||% range(y, finite = TRUE)
  graphics::plot.window(xlim = xlim, ylim = ylim, asp = asp)

  ## Evaluate pre-plot expressions
  panel.first

  ## Plot
  panel(x, y, name, ...)

  ## Evaluate post-plot and pre-axis expressions
  panel.last

  ## Construct Axis
  if (axes) {
    graphics::axis(side = 1, las = 1)
    graphics::axis(side = 2, las = 1)
  }

  ## Plot frame
  if (frame.plot) {
    graphics::box()
  }

  ## Add annotation
  if (ann) {
    graphics::title(main = main, sub = sub, xlab = xlab, ylab = ylab)
  }

  invisible(list(x, y))
}

#' Multiple Panels Plot
#'
#' @param x A [`numeric`] vector.
#' @param y A [`numeric`] matrix.
#' @param panel A [`function`] in the form `function(x, y, name, ...)`
#'  which gives the action to be carried out in each panel of the display
#'  (see [.plot_single()]).
#' @param y_flip A [`logical`] scalar: should the y-axis (ticks and numbering)
#'  be flipped from side 2 (left) to 4 (right) from series to series?
#' @param ncol An [`integer`] specifying the number of columns to use.
#'  Defaults to 1 for up to 4 series, otherwise to 2.
#' @inheritParams .plot_single
#' @return
#'  Called for its side-effects: it results in a graphic being displayed.
#'  Invisibly returns `x`.
#' @keywords internal
#' @noRd
.plot_multiple <- function(x, y, panel, ..., asp = NA,
                           y_flip = TRUE, n_col = NULL,
                           xlim = NULL, ylim = NULL,
                           xlab = NULL, ylab = NULL,
                           main = NULL, sub = NULL,
                           ann = graphics::par("ann"),
                           axes = TRUE, frame.plot = axes,
                           panel.first = NULL, panel.last = NULL) {
  m <- ncol(y)
  m_seq <- seq_len(m)
  if (is.null(n_col)) n_col <- if (m > 4) 2 else 1
  n_row <- ceiling(m / n_col)

  ## Graphical parameters
  ## Save and restore
  old_par <- graphics::par(
    mar = c(0, 5.1, 0, if (y_flip) 5.1 else 2.1),
    oma = c(6, 0, 5, 0),
    mfcol = c(n_row, n_col)
  )
  on.exit(graphics::par(old_par))

  dots <- list(...)
  cex.lab <- dots$cex.lab %||% graphics::par("cex.lab")
  col.lab <- dots$col.lab %||% graphics::par("col.lab")
  font.lab <- dots$font.lab %||% graphics::par("font.lab")
  cex.main <- dots$cex.main %||% graphics::par("cex.main")
  font.main <- dots$font.main %||% graphics::par("font.main")
  col.main <- dots$col.main %||% graphics::par("col.main")

  for (j in m_seq) {
    ## Plot
    yj <- y[, j, drop = TRUE]
    .plot_single(x = x, y = yj, name = colnames(y)[[j]], panel = panel,
                 asp = asp, xlim = xlim, ylim = ylim,
                 main = NULL, sub = NULL, ann = FALSE, axes = FALSE,
                 frame.plot = frame.plot,
                 panel.first = panel.first, panel.last = panel.last, ...)

    ## Construct Axis
    do_x <- (j %% n_row == 0 || j == m)
    y_side <- if (j %% 2 || !y_flip) 2 else 4
    if (axes) {
      if (do_x) {
        graphics::axis(side = 1, xpd = NA, las = 1)
      }
      graphics::axis(side = y_side, xpd = NA, las = 1)
    }

    ## Add annotation
    if (ann) {
      if (do_x) {
        graphics::mtext(xlab, side = 1, line = 3,
                        cex = cex.lab, col = col.lab, font = font.lab)
      }
      graphics::mtext(ylab, side = y_side, line = 3,
                      cex = cex.lab, col = col.lab, font = font.lab)
    }
  }

  ## Add annotation
  if (ann) {
    graphics::par(mfcol = c(1, 1))
    graphics::mtext(main, side = 3, line = 3,
                    cex = cex.main, font = font.main, col = col.main)
  }

  invisible(x)
}
