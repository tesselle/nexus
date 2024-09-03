# OUTLIERS
#' @include AllGenerics.R
NULL

# Find =========================================================================
#' @export
#' @rdname outliers
#' @aliases outliers,CompositionMatrix,missing-method
setMethod(
  f = "outliers",
  signature = c(object = "CompositionMatrix", reference = "missing"),
  definition = function(object, ..., method = c("mve", "mcd"), quantile = 0.975) {
    methods::callGeneric(object, reference = object,
                         method = method, quantile = quantile)
  }
)

#' @export
#' @rdname outliers
#' @aliases outliers,CompositionMatrix,CompositionMatrix-method
setMethod(
  f = "outliers",
  signature = c(object = "CompositionMatrix", reference = "CompositionMatrix"),
  definition = function(object, reference, ..., method = c("mve", "mcd"), quantile = 0.975) {
    ## Validation
    if (!all(colnames(object) == colnames(reference))) {
      stop("", call. = FALSE)
    }

    ## Transformation
    z <- transform_ilr(object)
    ref <- transform_ilr(reference)

    ## Clean
    n <- nrow(ref)
    p <- ncol(ref)
    if (n < (p + 1)) {
      msg <- "Sample size is too small (%d)."
      stop(sprintf(msg, n), call. = FALSE)
    }
    if (n < (2 * p)) {
      msg <- "Possibly too small sample size (%d)."
      warning(sprintf(msg, n), call. = FALSE)
    }

    ## Compute center and spread
    ## Standard estimators
    estc <- list(center = colMeans(ref, na.rm = TRUE), cov = cov(ref))

    ## Robust estimators
    method <- match.arg(method, several.ok = FALSE)
    estr <- MASS::cov.rob(ref, method = method, ...)

    ## Mahalanobis distance
    dc <- stats::mahalanobis(z, center = estc$center, cov = estc$cov)
    dr <- stats::mahalanobis(z, center = estr$center, cov = estr$cov)

    ## Threshold
    limit <- sqrt(stats::qchisq(p = quantile, df = p))

    .OutlierIndex(
      samples = rownames(z),
      groups = groups(z),
      standard = sqrt(dc),
      robust = sqrt(dr),
      limit = limit,
      dof = p
    )
  }
)

# Plot =========================================================================
#' @export
#' @method plot OutlierIndex
plot.OutlierIndex <- function(x, ...,
                              type = c("dotchart", "distance", "qqplot"),
                              robust = TRUE,
                              colors = color("discreterainbow"),
                              symbols = c(16, 1, 3),
                              xlim = NULL, ylim = NULL,
                              xlab = NULL, ylab = NULL,
                              main = NULL, sub = NULL,
                              ann = graphics::par("ann"),
                              axes = TRUE, frame.plot = axes,
                              panel.first = NULL, panel.last = NULL,
                              legend = list(x = "topleft")) {
  ## Get data
  dc <- x@standard
  dr <- x@robust
  grp <- x@groups

  ## Validation
  type <- match.arg(type, several.ok = FALSE)
  if (all(is.na(dr)) || all(is.na(dc))) {
    stop("No distances were calculated.", call. = FALSE)
  }

  dof <- x@dof
  limit <- x@limit
  n <- length(dc)

  ## Graphical parameters
  shape <- rep(symbols[[1L]], n)
  if (robust || type == "distance") shape[dr > limit] <- symbols[[2L]]
  shape[dc > limit] <- symbols[[3L]]
  col <- rep("black", length(grp))
  if (nlevels(grp) > 0) {
    col <- khroma::palette_color_discrete(colors)(grp)
  }

  asp <- NA
  cy <- if (robust) dr else dc
  ylab <- ylab %||% sprintf("%s Mahalanobis distance", ifelse(robust, "Robust", "Standard"))

  if (type == "dotchart") {
    cx <- seq_along(dc)
    xlab <- xlab %||% "Index"
    panel <- function() {
      graphics::points(x = cx, y = cy, pch = shape, col = col)
      graphics::abline(h = limit, lty = 1)
    }
  }
  if (type == "distance") {
    cx <- dc
    cy <- dr
    xlab <- xlab %||% "Standard Mahalanobis distance"
    ylab <- ylab %||% "Robust Mahalanobis distance"
    panel <- function() {
      graphics::points(x = cx, y = cy, pch = shape, col = col)
      graphics::abline(h = limit, lty = 1)
      graphics::abline(v = limit, lty = 1)
      graphics::abline(a = 0, b = 1, lty = 2, col = "darkgrey")
    }
  }
  if (type == "qqplot") {
    cx <- stats::qchisq(stats::ppoints(n), df = dof)
    xlab <- xlab %||% "Theoretrical Quantiles"
    asp <- 1
    panel <- function() {
      i <- order(cy)
      stats::qqline(
        y = cy,
        distribution = function(p) stats::qchisq(p, df = dof),
        probs = c(0.25, 0.75), col = "black"
      )
      graphics::points(x = cx, y = cy[i], pch = shape[i], col = col[i])
    }
  }

  ## Open new window
  grDevices::dev.hold()
  on.exit(grDevices::dev.flush(), add = TRUE)
  graphics::plot.new()

  ## Set plotting coordinates
  xlim <- xlim %||% range(cx, finite = TRUE)
  ylim <- ylim %||% range(cy, finite = TRUE)
  graphics::plot.window(xlim = xlim, ylim = ylim, asp = asp)

  ## Evaluate pre-plot expressions
  panel.first

  ## Plot
  panel()

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

  ## Add legend
  if (is.list(legend)) {
    if (robust || type == "distance") {
      lab <- c("No outlier", "Robust only", "Both")
    } else {
      lab <- c("No outlier", "Outlier")
      pch <- symbols[-2]
    }
    args <- list(x = "topleft", legend = lab, pch = symbols, bty = "n", xpd = NA)
    args <- utils::modifyList(args, legend)
    do.call(graphics::legend, args = args)
  }

  invisible(x)
}

#' @export
#' @rdname plot_outliers
#' @aliases plot,OutlierIndex,missing-method
setMethod("plot", c(x = "OutlierIndex", y = "missing"), plot.OutlierIndex)
