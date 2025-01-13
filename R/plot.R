# PLOT
#' @include AllGenerics.R
NULL

# LogRatio =====================================================================
#' @export
#' @method plot LogRatio
plot.LogRatio <- function(x, ..., factor = 1, amount = NULL,
                          palette_color = palette_color_discrete(),
                          palette_symbol = palette_shape(),
                          xlab = NULL, ylab = NULL,
                          main = NULL, sub = NULL, ann = graphics::par("ann"),
                          axes = TRUE, frame.plot = axes,
                          legend = list(x = "topright")) {
  ## Get data
  xy <- data.frame(
    x = jitter(as.vector(col(x)), factor = factor, amount = amount),
    y = as.vector(x)
  )

  ## Graphical parameters
  col <- list(...)$col %||% graphics::par("col")
  bg <- list(...)$bg %||% graphics::par("bg")
  pch <- list(...)$pch %||% graphics::par("pch")
  cex.axis <- list(...)$cex.axis %||% graphics::par("cex.axis")
  col.axis <- list(...)$col.axis %||% graphics::par("col.axis")
  font.axis <- list(...)$font.axis %||% graphics::par("font.axis")

  ## Grouping
  if (is_grouped(x)) {
    lvl <- group_names(x)
    col <- palette_color(lvl)
    bg <- grDevices::adjustcolor(col, alpha.f = 0.5)
    pch <- palette_symbol(lvl)
  }

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
    graphics::title(main = main, sub = sub, xlab = xlab, ylab = ylab, ...)
  }

  ## Add legend
  if (is.list(legend) && is_grouped(x)) {
    ## Compute legend position
    args <- list(x = "topright", legend = unique(lvl), pch = unique(pch),
                 col = unique(col), bg = unique(bg), bty = "n")
    args <- utils::modifyList(args, legend)

    ## Plot legend
    do.call(graphics::legend, args = args)
  }

  invisible(x)
}

#' @export
#' @rdname plot
#' @aliases plot,LogRatio,missing-method
setMethod("plot", c(x = "LogRatio", y = "missing"), plot.LogRatio)
