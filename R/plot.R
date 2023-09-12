# PLOT
#' @include AllGenerics.R
NULL

# CompositionMatrix ============================================================
#' @export
#' @method plot CompositionMatrix
plot.CompositionMatrix <- function(x, order = NULL, decreasing = FALSE,
                                   groups = get_groups(x), horiz = TRUE,
                                   xlab = NULL, ylab = NULL,
                                   main = NULL, sub = NULL,
                                   ann = graphics::par("ann"), axes = TRUE,
                                   ...) {
  ## Get data
  z <- x@.Data

  ## Ordering
  if (!is.null(order)) {
    ordering <- order(z[, order], decreasing = decreasing)
    z <- z[ordering, ]
  }

  ## Graphical parameters
  col <- list(...)$col %||% grDevices::hcl.colors(ncol(z), "viridis")
  cex.axis <- list(...)$cex.axis %||% graphics::par("cex.axis")
  col.axis <- list(...)$col.axis %||% graphics::par("col.axis")
  font.axis <- list(...)$font.axis %||% graphics::par("font.axis")
  cex.lab <- list(...)$cex.lab %||% graphics::par("cex.lab")
  col.lab <- list(...)$col.lab %||% graphics::par("col.lab")
  font.lab <- list(...)$font.lab %||% graphics::par("font.lab")
  cex.main <- list(...)$cex.main %||% graphics::par("cex.main")
  font.main <- list(...)$font.main %||% graphics::par("font.main")
  col.main <- list(...)$col.main %||% graphics::par("col.main")

  ## Grouping
  if (length(stats::na.omit(groups)) > 0) {
    arkhe::assert_length(groups, nrow(z))

    groups <- factor(groups, exclude = NULL)
    grp <- split(as.data.frame(z), f = groups)
    n <- nlevels(groups)

    ## Save and restore
    old_par <- graphics::par(
      mar = if (horiz) c(0, 5.1, 0, 1) else  c(5.1, 0, 0, 1),
      oma = if (horiz) c(6, 0, 5, 0) else c(0, 6, 5, 0),
      mfcol = if (horiz) c(n, 1) else c(1, n)
    )
    on.exit(graphics::par(old_par))

    x_side <- if (horiz) 1 else 2
    y_side <- if (horiz) 2 else 1

    for (i in seq_len(n)) {
      graphics::barplot(height = t(grp[[i]]), horiz = horiz, axes = FALSE,
                        main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
                        col = col, las = x_side, ...)

      ## Construct Axis
      do_x <- (horiz & i == n) | (!horiz & i == 1)
      if (axes) {
        if (do_x) {
          graphics::axis(side = x_side, cex.axis = cex.axis, col.axis = col.axis,
                         font.axis = font.axis, xpd = NA, las = 1)
        }
      }

      ## Add annotation
      if (ann) {
        ylab <- ylab %||% names(grp)
        if (do_x) {
          graphics::mtext(xlab, side = x_side, line = 3, cex = cex.lab,
                          col = col.lab, font = font.lab)
        }
        graphics::mtext(ylab[i], side = y_side, line = 3, cex = cex.lab,
                        col = col.lab, font = font.lab)
      }
    }

    ## Add annotation
    if (ann) {
      graphics::par(mfcol = c(1, 1))
      graphics::mtext(main, side = 3, line = 3, cex = cex.main, font = font.main,
                      col = col.main)
    }
  } else {
    graphics::barplot(height = t(z), horiz = horiz, col = col, las = 1,
                      main = main, sub = sub, xlab = xlab, ylab = ylab,
                      axes = axes, ann = ann, ...)
  }

  invisible(x)
}

#' @export
#' @rdname plot_coda
#' @aliases plot,CompositionMatrix,missing-method
setMethod("plot", c(x = "CompositionMatrix", y = "missing"), plot.CompositionMatrix)

# LogRatio =====================================================================
#' @export
#' @method plot LogRatio
plot.LogRatio <- function(x, order = NULL, decreasing = FALSE,
                          groups = get_groups(x), rug = TRUE, ticksize = 0.05,
                          ncol = NULL, flip = FALSE,
                          xlab = NULL, ylab = NULL,
                          main = NULL, ann = graphics::par("ann"),
                          axes = TRUE, frame.plot = axes,
                          legend = list(x = "topright"), ...) {
  ## Get data
  n_dens <- 512
  z <- x
  m <- nrow(z)
  p <- ncol(z)
  m_seq <- seq_len(m)
  p_seq <- seq_len(p)
  if (is.null(ncol)) ncol <- if (p > 4) 2 else 1
  nrow <- ceiling(p / ncol)

  ## Ordering
  if (!is.null(order)) {
    ordering <- order(z[, order], decreasing = decreasing)
    z <- z[ordering, ]
  }

  ## Grouping
  if (length(stats::na.omit(groups)) == 0) {
    groups <- rep(NA, m)
  } else {
    rug <- FALSE
  }
  arkhe::assert_length(groups, m)
  groups <- factor(groups, exclude = NULL)
  k <- nlevels(groups)
  grp <- split(m_seq, f = groups)

  ## Graphical parameters
  ## Save and restore
  old_par <- graphics::par(
    mar = c(0, 5.1, 0, if (flip) 5.1 else 2.1),
    oma = c(6, 0, 5, 0),
    mfcol = c(nrow, ncol)
  )
  on.exit(graphics::par(old_par))

  col <- list(...)$col
  border <- list(...)$border
  lty <- list(...)$lty %||% graphics::par("lty")
  cex.axis <- list(...)$cex.axis %||% graphics::par("cex.axis")
  col.axis <- list(...)$col.axis %||% graphics::par("col.axis")
  font.axis <- list(...)$font.axis %||% graphics::par("font.axis")
  cex.lab <- list(...)$cex.lab %||% graphics::par("cex.lab")
  col.lab <- list(...)$col.lab %||% graphics::par("col.lab")
  font.lab <- list(...)$font.lab %||% graphics::par("font.lab")
  cex.main <- list(...)$cex.main %||% graphics::par("cex.main")
  font.main <- list(...)$font.main %||% graphics::par("font.main")
  col.main <- list(...)$col.main %||% graphics::par("col.main")

  if (is.null(border)) border <- grDevices::hcl.colors(k, "viridis")
  if (is.null(col)) col <- grDevices::adjustcolor(border, alpha.f = 0.5)

  ## Compute densities
  dens_x <- dens_y <- array(data = NA_real_, dim = c(n_dens, p, k),
                            dimnames = list(NULL, NULL, levels(groups)))
  for (i in 1:k) {
    for (j in p_seq) {
      tmp <- z[grp[[i]], j, drop = TRUE]
      dens <- stats::density(x = tmp, n = n_dens)
      dens_x[, j, i] <- dens$x
      dens_y[, j, i] <- dens$y
    }
  }

  xlim <- range(dens_x)
  ylabs <- ylab %||% colnames(z) %||% paste0("P", p_seq)
  for (j in p_seq) {
    tmp_x <- dens_x[, j, , drop = FALSE]
    tmp_y <- dens_y[, j, , drop = FALSE]

    ## Open new window
    grDevices::dev.hold()
    on.exit(grDevices::dev.flush(), add = TRUE)
    graphics::plot.new()

    ## Set plotting coordinates
    ylim <- c(0, max(tmp_y) * 1.05)
    graphics::plot.window(xlim = xlim, ylim = ylim, yaxs = "i")

    ## Evaluate pre-plot expressions
    # panel.first

    ## Plot
    for (i in 1:k) {
      graphics::polygon(x = tmp_x[, , i], y = tmp_y[, , i], col = col[i],
                        border = border[i], lty = lty)
    }
    if (rug) {
      graphics::rug(z[, j, drop = TRUE], ticksize = ticksize, side = 1)
    }

    ## Evaluate post-plot and pre-axis expressions
    # panel.last

    ## Construct Axis
    do_x <- (j %% nrow == 0 || j == m)
    y_side <- if (j %% 2 || !flip) 2 else 4
    if (axes) {
      if (do_x) {
        graphics::axis(side = 1, cex.axis = cex.axis, col.axis = col.axis,
                       font.axis = font.axis, xpd = NA, las = 1)
      }
      graphics::axis(side = y_side, cex.axis = cex.axis, col.axis = col.axis,
                     font.axis = font.axis, xpd = NA, las = 1)
    }

    ## Plot frame
    if (frame.plot) {
      graphics::box()
    }

    ## Add annotation
    if (ann) {
      if (do_x) {
        graphics::mtext(xlab, side = 1, line = 3, cex = cex.lab, col = col.lab,
                        font = font.lab)
      }
      graphics::mtext(ylabs[[j]], side = y_side, line = 3, cex = cex.lab,
                      col = col.lab, font = font.lab)
    }
  }

  ## Add legend
  if (is.list(legend) && length(legend) > 0 && k > 1) {
    args <- list(legend = levels(groups), col = border, fill = col, bty = "n")
    args <- utils::modifyList(args, legend)
    do.call(graphics::legend, args = args)
  }

  ## Add annotation
  if (ann) {
    graphics::par(mfcol = c(1, 1))
    graphics::mtext(main, side = 3, line = 3, cex = cex.main, font = font.main,
                    col = col.main)
  }

  invisible(x)
}

#' @export
#' @rdname plot_logratio
#' @aliases plot,LogRatio,missing-method
setMethod("plot", c(x = "LogRatio", y = "missing"), plot.LogRatio)
