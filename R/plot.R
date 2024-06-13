# PLOT
#' @include AllGenerics.R
NULL

# CompositionMatrix ============================================================
#' @export
#' @method plot CompositionMatrix
plot.CompositionMatrix <- function(x, ..., margin = NULL, groups = get_groups(x)) {
  if (!is.null(groups) && !all(is.na(groups))) {
    col <- dimensio::palette_color_discrete(list(...)$col)(groups)
    pch <- dimensio::palette_shape(list(...)$pch)(groups)
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
plot.LogRatio <- function(x, ...,
                          groups = get_groups(x), rug = TRUE, ticksize = 0.05,
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
  if (is.null(groups) || all(is.na(groups))) {
    grp <- list(all = z)
    groups <- rep("all", m)
  } else {
    arkhe::assert_length(groups, m)
    grp <- split(z, f = groups)
    rug <- FALSE
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
  border <- dimensio::palette_color_discrete(list(...)$border)(names(grp))
  col <- list(...)$col %||% grDevices::adjustcolor(border, alpha.f = 0.5)

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
  ylabs <- ylab %||% colnames(z) %||% paste0("P", p_seq)
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

# OutlierIndex =================================================================
#' @export
#' @method plot OutlierIndex
plot.OutlierIndex <- function(x, ...,
                              type = c("dotchart", "qqplot"),
                              pch.in = 16, pch.out = 3,
                              ncol = NULL, flip = FALSE,
                              xlab = NULL, ylab = NULL,
                              main = NULL, sub = NULL,
                              ann = graphics::par("ann"),
                              axes = TRUE, frame.plot = axes,
                              panel.first = NULL, panel.last = NULL,
                              legend = list(x = "top")) {
  ## Get data
  n <- nrow(x)
  m <- ncol(x)

  dof <- x@dof
  limit <- x@limit
  grp <- get_groups(x)

  pch <- rep(pch.in, n)
  if (!any_assigned(x)) {
    col <- rep("black", n)
  } else {
    col <- dimensio::palette_color_discrete(list(...)$col)(grp)
  }

  ## Plot type
  type <- match.arg(type, several.ok = FALSE)
  cx <- seq_len(n)
  cy <- x
  if (type == "qqplot") cx <- stats::qchisq(stats::ppoints(n), df = dof)

  panel <- switch(
    type,
    dotchart = function(x, y, name, ...) {
      shape <- pch
      shape[y > limit] <- pch.out

      assigned <- grp == name
      if (all(is.na(assigned)) || !any(assigned)) assigned <- rep(TRUE, length(x))

      color <- col
      color[!assigned] <- "#DDDDDD"

      graphics::points(x = x, y = y, pch = shape, col = color, ...)
      graphics::abline(h = limit, lty = 2)
    },
    qqplot = function(x, y, name, probs = c(0.25, 0.75), ...) {
      i <- order(y)
      y <- y[i]

      shape <- pch
      shape[y > limit] <- pch.out

      assigned <- grp[i] == name
      if (all(is.na(assigned)) || !any(assigned)) assigned <- rep(TRUE, length(x))

      color <- col[i]
      color[!assigned] <- "#DDDDDD"

      stats::qqline(
        y = y[assigned],
        distribution = function(p) stats::qchisq(p, df = dof),
        probs = probs, col = "black", ...
      )
      graphics::points(x = x, y = y, pch = shape, col = color, ...)
    }
  )

  asp <- if (type == "qqplot") 1 else NA
  rob <- if (x@robust) "Robust" else "Standard"
  xlab <- xlab %||% if (type == "qqplot") "Theoretrical Quantiles" else "Index"
  ylab <- ylab %||% sprintf("%s Mahalanobis distance", rob)

  if (m > 1) {
    .plot_multiple(x = cx, y = cy, panel = panel,
                   asp = asp, y_flip = flip, n_col = ncol, ...,
                   xlab = xlab, ylab = ylab,
                   main = main, sub = sub,
                   ann = ann, axes = axes,
                   frame.plot = frame.plot, panel.first = panel.first,
                   panel.last = panel.last)

    ## Add legend
    # https://stackoverflow.com/a/42076830
    if (is.list(legend)) {
      leg_par <- graphics::par(mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0),
                               mfcol = c(1, 1), new = TRUE)
      on.exit(graphics::par(leg_par), add = TRUE)
      graphics::plot(0, 0, type = "n", ann = FALSE, axes = FALSE)

      ## Compute legend position
      args <- list(x = "top", legend = unique(grp), fill = unique(col),
                   ncol = ceiling(m / 2), bty = "n", xpd = NA)
      args <- utils::modifyList(args, legend)

      ## Plot legend
      do.call(graphics::legend, args = args)
    }
  } else {
    .plot_single(x = cx, y = cy[, 1, drop = TRUE],
                 name = "", panel = panel, ..., asp = asp,
                 xlab = xlab, ylab = ylab,
                 main = main, sub = sub,
                 ann = ann, axes = axes,
                 frame.plot = frame.plot, panel.first = panel.first,
                 panel.last = panel.last)
  }

  invisible(x)
}

#' @export
#' @rdname plot_outliers
#' @aliases plot,OutlierIndex,missing-method
setMethod("plot", c(x = "OutlierIndex", y = "missing"), plot.OutlierIndex)
