# OUTLIERS
#' @include AllGenerics.R
NULL

# Find =========================================================================
#' @export
#' @rdname outliers
#' @aliases outliers,CompositionMatrix-method
setMethod(
  f = "outliers",
  signature = c(object = "CompositionMatrix"),
  definition = function(object, groups = get_groups(object), robust = TRUE,
                        method = c("mve", "mcd"), quantile = 0.975, ...) {
    ## Transformation
    z <- transform_ilr(object)

    ## Grouping
    m <- nrow(z)
    p <- ncol(z)
    if (is.null(groups)) groups <- rep(NA, m)
    arkhe::assert_length(groups, m)
    groups <- factor(groups, exclude = NULL)
    k <- nlevels(groups)
    grp <- split(seq_len(m), f = groups)

    d2 <- matrix(data = NA_real_, nrow = m, ncol = k)
    dimnames(d2) <- list(rownames(object), levels(groups))

    ## For each group...
    for (i in seq_len(k)) {
      ## ...subset
      tmp <- z[grp[[i]], , drop = FALSE]

      ## ...compute center and spread
      if (!robust) method <- "classical" # Standard estimators
      else method <- match.arg(method, several.ok = FALSE) # Robust estimators
      est <- MASS::cov.rob(tmp, method = method, ...)

      ## Mahalanobis distance
      d2[, i] <- stats::mahalanobis(z, center = est$center, cov = est$cov)
    }

    ## Threshold
    dof <- p - 1L
    limit <- sqrt(stats::qchisq(p = quantile, df = dof))

    d <- sqrt(d2)
    out <- d > limit

    .OutlierIndex(
      out,
      samples = get_samples(object),
      groups = as.character(groups),
      distances = d,
      limit = limit,
      robust = robust,
      dof = dof
    )
  }
)

# Plot =========================================================================
#' @export
#' @method plot OutlierIndex
plot.OutlierIndex <- function(x, qq = FALSE, probs = c(0.25, 0.75),
                              ncol = NULL, flip = FALSE,
                              xlab = NULL, ylab = NULL,
                              main = NULL, sub = NULL,
                              ann = graphics::par("ann"),
                              axes = TRUE, frame.plot = axes,
                              panel.first = NULL, panel.last = NULL, ...) {
  ## Get data
  d <- x@distances
  g <- x@groups
  k <- length(unique(g))

  ## Annotation
  xlab <- xlab %||% ifelse(qq, "Theoretrical Quantiles", "Index")

  if (k > 1) {
    if (is.null(ncol)) ncol <- if (k > 4) 2 else 1
    nrow <- ceiling(k / ncol)
    ylabs <- ylab %||% colnames(d)

    ## Graphical parameters
    ## Save and restore
    old_par <- graphics::par(
      mar = c(0, 5.1, 0, if (flip) 5.1 else 2.1),
      oma = c(6, 0, 5, 0),
      mfcol = c(nrow, ncol)
    )
    on.exit(graphics::par(old_par))

    cex.axis <- list(...)$cex.axis %||% graphics::par("cex.axis")
    col.axis <- list(...)$col.axis %||% graphics::par("col.axis")
    font.axis <- list(...)$font.axis %||% graphics::par("font.axis")
    cex.lab <- list(...)$cex.lab %||% graphics::par("cex.lab")
    col.lab <- list(...)$col.lab %||% graphics::par("col.lab")
    font.lab <- list(...)$font.lab %||% graphics::par("font.lab")
    cex.main <- list(...)$cex.main %||% graphics::par("cex.main")
    font.main <- list(...)$font.main %||% graphics::par("font.main")
    col.main <- list(...)$col.main %||% graphics::par("col.main")

    ## Plot
    for (j in seq_len(k)) {
      dj <- d[, j, drop = TRUE]
      gj <- g == colnames(d)[j]
      .plot_outliers(dj, df = x@dof, qq = qq, qqline = gj,
                     probs = probs, limit = x@limit,
                     xlab = NULL, ylab = NULL, main = NULL, sub = NULL,
                     ann = FALSE, axes = FALSE, frame.plot = frame.plot, ...)

      ## Construct Axis
      do_x <- (j %% nrow == 0 || j == k)
      y_side <- if (j %% 2 || !flip) 2 else 4
      if (axes) {
        if (do_x) {
          graphics::axis(side = 1, cex.axis = cex.axis, col.axis = col.axis,
                         font.axis = font.axis, xpd = NA, las = 1)
        }
        graphics::axis(side = y_side, cex.axis = cex.axis, col.axis = col.axis,
                       font.axis = font.axis, xpd = NA, las = 1)
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

    ## Add annotation
    if (ann) {
      graphics::par(mfcol = c(1, 1))
      graphics::mtext(main, side = 3, line = 3, cex = cex.main, font = font.main,
                      col = col.main)
    }
  } else {
    rob <- ifelse(x@robust, "Robust", "Standard")
    ylab <- ylab %||% sprintf("%s Mahalanobis distance", rob)

    .plot_outliers(d[, 1], df = x@dof, qq = qq, limit = x@limit, probs = probs,
                   xlab = xlab, ylab = ylab, main = main, sub = sub,
                   ann = ann, axes = axes, frame.plot = frame.plot,
                   panel.first = panel.first, panel.last = panel.last, ...)
  }

  invisible(x)
}

#' @export
#' @rdname plot_outliers
#' @aliases plot,OutlierIndex,missing-method
setMethod("plot", c(x = "OutlierIndex", y = "missing"), plot.OutlierIndex)

.plot_outliers <- function(x, df, qq = FALSE, qqline = seq_along(x),
                           probs = c(0.25, 0.75), limit = NULL,
                           col.group = "black", col.samples = "grey",
                           pch.group = 16, pch.samples = 1,
                           xlab = NULL, ylab = NULL,
                           main = NULL, sub = NULL,
                           ann = graphics::par("ann"),
                           axes = TRUE, frame.plot = axes,
                           panel.first = NULL, panel.last = NULL, ...) {
  ## Prepare data
  n <- length(x)
  index <- seq_len(n)

  khi <- stats::qchisq(stats::ppoints(n), df = df)
  i <- if (qq) order(x) else index
  data_x <- if (qq) khi else index
  data_y <- x[i]

  ## Open new window
  grDevices::dev.hold()
  on.exit(grDevices::dev.flush(), add = TRUE)
  graphics::plot.new()

  ## Set plotting coordinates
  xlim <- range(data_x, na.rm = TRUE, finite = TRUE)
  ylim <- range(data_y, na.rm = TRUE, finite = TRUE)
  graphics::plot.window(xlim = xlim, ylim = ylim, asp = if (qq) 1 else NA)

  ## Evaluate pre-plot expressions
  panel.first

  ## Plot
  col <- rep(col.samples, n)
  col[qqline[i]] <- col.group
  pch <- rep(pch.samples, n)
  pch[qqline[i]] <- pch.group

  graphics::points(x = data_x, y = data_y, col = col, pch = pch, ...)
  if (qq) {
    stats::qqline(
      y = data_y[qqline[i]],
      distribution = function(p) stats::qchisq(p, df = df),
      probs = probs, col = "#BB5566", ...
    )
  }
  if (!qq && is.numeric(limit)) graphics::abline(h = limit, lty = 2)

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

  invisible(x)
}
