# OUTLIERS
#' @include AllClasses.R AllGenerics.R
NULL

# Find =========================================================================
#' @export
#' @rdname outliers
#' @aliases outliers,CompositionMatrix-method
setMethod(
  f = "outliers",
  signature = signature(object = "CompositionMatrix"),
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
#' @method qqplot OutlierIndex
qqplot.OutlierIndex <- function(x, xlab = NULL, ylab = NULL, ...,
                                probs = c(0.25, 0.75),
                                main = NULL, sub = NULL,
                                col.points = "black", col.line = "red",
                                conf.level = NULL,
                                conf.args = list(exact = NULL, simulate.p.value = FALSE,
                                                 B = 2000, col = NA, border = NULL)) {
  ## Prepare data
  data <- as.data.frame(x)
  dof <- x@df
  qq <- stats::qchisq(stats::ppoints(nrow(data)), df = dof)
  spl <- data$distance

  ## Plot
  xlab <- xlab %||% "Theoretrical Quantiles"
  rob <- ifelse(x@robust, "Robust", "Standard")
  ylab <- ylab %||% sprintf("%s Mahalanobis distance", rob)
  stats::qqplot(x = qq, y = spl, col = col.points,
                xlab = xlab, ylab = ylab, main = main, sub = sub,
                las = 1, ..., conf.level = conf.level, conf.args = conf.args)
  stats::qqline(y = spl, distribution = function(p) stats::qchisq(p, df = dof),
                probs = probs, col = col.line, ...)

  invisible(x)
}

#' @export
#' @rdname plot_outliers
#' @aliases qqplot,OutlierIndex,missing-method
setMethod("qqplot", c(x = "OutlierIndex", y = "missing"), qqplot.OutlierIndex)

#' @export
#' @method plot OutlierIndex
plot.OutlierIndex <- function(x, limit = TRUE,
                              col = c("#004488", "#DDAA33"),
                              pch = c(15, 16),
                              xlab = NULL, ylab = NULL,
                              main = NULL, sub = NULL,
                              ann = graphics::par("ann"),
                              axes = TRUE, frame.plot = axes,
                              panel.first = NULL, panel.last = NULL, ...) {
  ## Prepare data
  data <- as.data.frame(x)

  ## Open new window
  grDevices::dev.hold()
  on.exit(grDevices::dev.flush(), add = TRUE)
  graphics::plot.new()

  ## Set plotting coordinates
  xlim <- range(data$index)
  ylim <- range(data$distance)
  graphics::plot.window(xlim = xlim, ylim = ylim)

  ## Evaluate pre-plot expressions
  panel.first

  ## Plot
  col <- col[as.factor(data$outlier)]
  pch <- pch[as.factor(data$outlier)]
  graphics::points(x = data$index, y = data$distance, col = col, pch = pch, ...)
  if (limit) graphics::abline(h = x@limit, lty = 2)

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
    xlab <- xlab %||% "Index"
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
