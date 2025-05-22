# PLOT
#' @include AllGenerics.R
NULL

# LogRatio =====================================================================
#' @export
#' @method plot LogRatio
plot.LogRatio <- function(x, ..., jitter_factor = 1, jitter_amount = NULL,
                          xlab = NULL, ylab = NULL,
                          main = NULL, sub = NULL, ann = graphics::par("ann"),
                          axes = TRUE, frame.plot = axes) {
  ## Get data
  xy <- data.frame(
    x = jitter(as.vector(col(x)), factor = jitter_factor, amount = jitter_amount),
    y = as.vector(x)
  )

  ## Graphical parameters
  col <- list(...)$col %||% graphics::par("col")
  bg <- list(...)$bg %||% graphics::par("bg")
  pch <- list(...)$pch %||% graphics::par("pch")
  cex.axis <- list(...)$cex.axis %||% graphics::par("cex.axis")
  col.axis <- list(...)$col.axis %||% graphics::par("col.axis")
  font.axis <- list(...)$font.axis %||% graphics::par("font.axis")

  ## Open new window
  grDevices::dev.hold()
  on.exit(grDevices::dev.flush(), add = TRUE)
  graphics::plot.new()

  ## Set plotting coordinates
  xlim <- range(xy$x)
  ylim <- range(xy$y)
  graphics::plot.window(xlim = xlim, ylim = ylim)

  ## Plot
  graphics::points(x = xy$x, y = xy$y, pch = pch, col = col, bg = bg)

  ## Construct axis
  if (axes) {
    graphics::axis(side = 1, at = seq_len(ncol(x)),
                   labels = labels(x), las = 1,
                   cex.axis = cex.axis, col.axis = col.axis,
                   font.axis = font.axis)
    graphics::axis(side = 2, las = 1)
  }

  ## Plot frame
  if (frame.plot) {
    graphics::box()
  }

  ## Add annotation
  if (ann) {
    ylab <- ylab %||% get_transformation(x)
    graphics::title(main = main, sub = sub, xlab = xlab, ylab = ylab)
  }

  invisible(x)
}

#' @export
#' @rdname plot
#' @aliases plot,LogRatio,missing-method
setMethod("plot", c(x = "LogRatio", y = "missing"), plot.LogRatio)

#' @export
#' @method plot GroupedLogRatio
plot.GroupedLogRatio <- function(x, ..., jitter_factor = 1, jitter_amount = NULL,
                                 color = NULL, symbol = NULL,
                                 xlab = NULL, ylab = NULL,
                                 main = NULL, sub = NULL,
                                 ann = graphics::par("ann"),
                                 axes = TRUE, frame.plot = axes,
                                 legend = list(x = "topright")) {

  ## Graphical parameters
  lvl <- group_names(x)
  col <- khroma::palette_color_discrete(color)(lvl)
  bg <- grDevices::adjustcolor(col, alpha.f = 0.5)
  pch <- khroma::palette_shape(symbol)(lvl)

  ## Plot
  plot(
    ungroup(x),
    col = col, pch = pch, bg = bg,
    jitter_factor = jitter_factor,
    jitter_amount = jitter_amount,
    xlab = xlab, ylab = ylab,
    main = main, sub = sub, ann = ann,
    axes = axes, frame.plot = frame.plot
  )

  ## Add legend
  if (is.list(legend) && is_grouped(x)) {
    args <- list(
      x = "topright",
      legend = tapply(lvl, lvl, unique),
      pch = tapply(pch, lvl, unique),
      col = tapply(col, lvl, unique),
      bg = tapply(bg, lvl, unique),
      bty = "n"
    )
    args <- utils::modifyList(args, legend)
    do.call(graphics::legend, args = args)
  }

  invisible(x)
}

#' @export
#' @rdname plot
#' @aliases plot,GroupedLogRatio,missing-method
setMethod("plot", c(x = "GroupedLogRatio", y = "missing"), plot.GroupedLogRatio)
