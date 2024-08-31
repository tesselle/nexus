# PLOT
#' @include AllGenerics.R
NULL

# CompositionMatrix ============================================================
#' @export
#' @method plot CompositionMatrix
plot.CompositionMatrix <- function(x, ..., margin = NULL, groups = group(x),
                                   palette_color = palette_color_discrete(),
                                   palette_symbol = palette_shape()) {
  ## Grouping
  if (has_groups(groups)) {
    arkhe::assert_length(groups, nrow(x))
    col <- palette_color(groups)
    pch <- palette_symbol(groups)
  } else {
    col <- list(...)$col %||% graphics::par("col")
    pch <- list(...)$pch %||% graphics::par("pch")
  }

  isopleuros::ternary_pairs(x, margin = margin, col = col, pch = pch, ...)
  invisible(x)
}

#' @export
#' @rdname plot
#' @aliases plot,CompositionMatrix,missing-method
setMethod("plot", c(x = "CompositionMatrix", y = "missing"), plot.CompositionMatrix)

# LogRatio =====================================================================
#' @export
#' @method plot LogRatio
plot.LogRatio <- function(x, ..., groups = group(x),
                          palette_color = palette_color_discrete(),
                          rug = TRUE, ticksize = 0.05,
                          ncol = NULL, flip = FALSE,
                          xlab = NULL, ylab = NULL,
                          main = NULL, ann = graphics::par("ann"),
                          axes = TRUE, frame.plot = axes,
                          legend = list(x = "top")) {
  ## Get data
  z <- x
  m <- nrow(z)
  p <- ncol(z)
  m_seq <- seq_len(m)
  p_seq <- seq_len(p)
  if (is.null(ncol)) ncol <- if (p > 4) 2 else 1
  nrow <- ceiling(p / ncol)

  ## Grouping
  if (has_groups(groups)) {
    arkhe::assert_length(groups, m)
    grp <- split(z, f = groups)
    border <- palette_color(names(grp))
    rug <- FALSE
  } else {
    grp <- list(all = z)
    groups <- rep("all", m)
    border <- list(...)$border %||% graphics::par("col")
  }
  k <- length(grp)

  ## Graphical parameters
  ## Save and restore
  old_par <- graphics::par(
    mar = c(0, 5.1, 0, if (flip) 5.1 else 2.1),
    oma = c(6, 0, 5, 0),
    mfcol = c(nrow, ncol)
  )
  on.exit(graphics::par(old_par))

  cex.lab <- list(...)$cex.lab %||% graphics::par("cex.lab")
  col.lab <- list(...)$col.lab %||% graphics::par("col.lab")
  font.lab <- list(...)$font.lab %||% graphics::par("font.lab")
  cex.main <- list(...)$cex.main %||% graphics::par("cex.main")
  font.main <- list(...)$font.main %||% graphics::par("font.main")
  col.main <- list(...)$col.main %||% graphics::par("col.main")

  lty <- list(...)$lty %||% graphics::par("lty")
  col <- grDevices::adjustcolor(border, alpha.f = 0.25)

  ## Compute densities
  n_dens <- 512
  dens_x <- dens_y <- array(data = NA_real_, dim = c(n_dens, p, k),
                            dimnames = list(NULL, NULL, names(grp)))
  for (i in seq_len(k)) {
    for (j in p_seq) {
      tmp <- grp[[i]][, j, drop = TRUE]
      dens <- stats::density(x = tmp, n = n_dens)
      dens_x[, j, i] <- dens$x
      dens_y[, j, i] <- dens$y
    }
  }

  xlim <- range(dens_x, na.rm = TRUE)
  ylabs <- ylab %||% labels(z) %||% paste0("z", p_seq)
  for (j in p_seq) {
    tmp_x <- dens_x[, j, , drop = FALSE]
    tmp_y <- dens_y[, j, , drop = FALSE]

    ## Open new window
    grDevices::dev.hold()
    on.exit(grDevices::dev.flush(), add = TRUE)
    graphics::plot.new()

    ## Set plotting coordinates
    ylim <- c(0, max(tmp_y, na.rm = TRUE) * 1.05)
    graphics::plot.window(xlim = xlim, ylim = ylim, yaxs = "i")

    ## Plot
    for (i in 1:k) {
      graphics::polygon(x = tmp_x[, , i], y = tmp_y[, , i], col = col[i],
                        border = border[i], lty = lty)
    }
    if (rug) {
      graphics::rug(z[, j, drop = TRUE], ticksize = ticksize, side = 1)
    }

    ## Construct Axis
    do_x <- (j %% nrow == 0 || j == p)
    y_side <- if (j %% 2 || !flip) 2 else 4
    if (axes) {
      if (do_x) {
        graphics::axis(side = 1, xpd = NA, las = 1)
      }
      graphics::axis(side = y_side, xpd = NA, las = 1)
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
  if (is.list(legend) && k > 1) {
    leg_par <- graphics::par(mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0),
                             mfcol = c(1, 1), new = TRUE)
    on.exit(graphics::par(leg_par), add = TRUE)
    graphics::plot(0, 0, type = "n", ann = FALSE, axes = FALSE)

    ## Compute legend position
    args <- list(x = "top", legend = names(grp), col = border, fill = col,
                 ncol = ceiling(m / 2), bty = "n", xpd = NA)
    args <- utils::modifyList(args, legend)

    ## Plot legend
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
