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
  definition = function(object, center = NULL, cov = NULL, robust = TRUE,
                        alpha = 0.5, level = 0.975) {

    df <- ncol(object) - 1L
    distance <- sqrt(mahalanobis(object, center = center, cov = cov,
                                 robust = robust, alpha = alpha))
    limit <- sqrt(stats::qchisq(p = level, df = df))
    out <- distance > limit
    names(out) <- rownames(object)

    .OutlierIndex(
      out,
      samples = get_samples(object),
      groups = get_groups(object),
      distances = distance,
      limit = limit,
      robust = robust,
      df = df
    )
  }
)

# Plot =========================================================================
#' @export
#' @method plot OutlierIndex
plot.OutlierIndex <- function(x, qq = FALSE, limit = TRUE,
                              probs = c(0.25, 0.75),
                              col = c("#004488", "#DDAA33"),
                              pch = c(15, 16),
                              xlab = NULL, ylab = NULL,
                              main = NULL, sub = NULL,
                              ann = graphics::par("ann"),
                              axes = TRUE, frame.plot = axes,
                              panel.first = NULL, panel.last = NULL, ...) {
  ## Prepare data
  data <- as.data.frame(x)
  dof <- x@df
  khi <- stats::qchisq(stats::ppoints(nrow(data)), df = dof)
  i <- if (qq) order(data$distance) else seq_len(nrow(data))
  data_x <- if (qq) khi else data$index
  data_y <- data$distance[i]

  ## Open new window
  grDevices::dev.hold()
  on.exit(grDevices::dev.flush(), add = TRUE)
  graphics::plot.new()

  ## Set plotting coordinates
  xlim <- range(data_x)
  ylim <- range(data_y)
  graphics::plot.window(xlim = xlim, ylim = ylim, asp = 1)

  ## Evaluate pre-plot expressions
  panel.first

  ## Plot
  col <- col[as.factor(data$outlier[i])]
  pch <- pch[as.factor(data$outlier[i])]
  graphics::points(x = data_x, y = data_y, col = col, pch = pch, ...)
  if (qq) {
    stats::qqline(
      y = data_y,
      distribution = function(p) stats::qchisq(p, df = dof),
      probs = probs, col = "red", ...
    )
  }
  if (!qq && limit) graphics::abline(h = x@limit, lty = 2)

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
    xlab <- xlab %||% ifelse(qq, "Theoretrical Quantiles", "Index")
    rob <- ifelse(x@robust, "Robust", "Standard")
    ylab <- ylab %||% sprintf("%s Mahalanobis distance", rob)
    graphics::title(main = main, sub = sub, xlab = xlab, ylab = ylab)
  }

  invisible(x)
}

#' @export
#' @rdname plot_outliers
#' @aliases plot,OutlierIndex,missing-method
setMethod("plot", c(x = "OutlierIndex", y = "missing"), plot.OutlierIndex)
