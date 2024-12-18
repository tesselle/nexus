# OUTLIERS
#' @include AllGenerics.R
NULL

# Find =========================================================================
#' @export
#' @rdname detect_outlier
#' @aliases detect_outlier,CompositionMatrix,missing-method
setMethod(
  f = "detect_outlier",
  signature = c(object = "CompositionMatrix", reference = "missing"),
  definition = function(object, ..., robust = TRUE, method = c("mve", "mcd"),
                        quantile = 0.975) {
    methods::callGeneric(object, reference = object, robust = robust,
                         method = method, quantile = quantile)
  }
)

#' @export
#' @rdname detect_outlier
#' @aliases detect_outlier,CompositionMatrix,CompositionMatrix-method
setMethod(
  f = "detect_outlier",
  signature = c(object = "CompositionMatrix", reference = "CompositionMatrix"),
  definition = function(object, reference, ..., robust = TRUE,
                        method = c("mve", "mcd"), quantile = 0.975) {
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

    ## Compute center and spread + Mahalanobis distance
    ## Standard estimators
    estc <- list(center = colMeans(ref, na.rm = TRUE), cov = cov(ref))
    dc <- stats::mahalanobis(z, center = estc$center, cov = estc$cov)

    ## Robust estimators
    dr <- rep(NA_real_, nrow(z))
    if (robust) {
      method <- match.arg(method, several.ok = FALSE)
      estr <- MASS::cov.rob(ref, method = method, ...)
      dr <- stats::mahalanobis(z, center = estr$center, cov = estr$cov)
    }

    ## Threshold
    limit <- sqrt(stats::qchisq(p = quantile, df = p))

    z <- .OutlierIndex(
      samples = rownames(z),
      standard = sqrt(dc),
      robust = sqrt(dr),
      limit = limit,
      dof = p
    )
  }
)

#' @export
#' @rdname detect_outlier
#' @aliases is_outlier,OutlierIndex-method
setMethod(
  f = "is_outlier",
  signature = c("OutlierIndex"),
  definition = function(object, robust = TRUE) {
    d <- if (robust) object@robust else object@standard
    out <- d > object@limit
    names(out) <- object@samples
    out
  }
)

# Plot =========================================================================
#' @export
#' @method plot OutlierIndex
plot.OutlierIndex <- function(x, ...,
                              type = c("dotchart", "distance"),
                              robust = TRUE,
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
  dof <- x@dof
  limit <- x@limit
  n <- length(dc)

  ## Validation
  if (all(is.na(dr))) {
    robust <- FALSE
    type <- "dotchart"
  }
  type <- match.arg(type, several.ok = FALSE)

  ## Graphical parameters
  shape <- rep(symbols[[1L]], n)
  if (robust || type == "distance") shape[dr > limit] <- symbols[[3L]]
  if (!robust || type == "distance") shape[dc > limit] <- symbols[[2L]]

  cy <- if (robust) dr else dc
  dlab <- ifelse(robust, "Robust Mahalanobis distance", "Standard Mahalanobis distance")
  ylab <- ylab %||% dlab

  if (type == "dotchart") {
    asp <- NA
    cx <- seq_along(dc)
    xlab <- xlab %||% "Index"
    panel <- function() {
      graphics::points(x = cx, y = cy, pch = shape, ...)
      graphics::abline(h = limit, lty = 1)
    }
  }
  if (type == "distance") {
    asp <- 1
    cx <- dc
    cy <- dr
    xlab <- xlab %||% "Standard Mahalanobis distance"
    ylab <- ylab %||% "Robust Mahalanobis distance"
    panel <- function() {
      graphics::points(x = cx, y = cy, pch = shape, ...)
      graphics::abline(h = limit, lty = 1)
      graphics::abline(v = limit, lty = 1)
      graphics::abline(a = 0, b = 1, lty = 2, col = "darkgrey")
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
    if (type == "distance") {
      lab <- c("No outlier", "Robust only", "Both")
      pch <- symbols
    } else {
      lab <- c("No outlier", "Outlier")
      pch <- symbols[-2 - !robust]
    }
    args <- list(x = "topleft", legend = lab, pch = pch, bty = "n", xpd = NA)
    args <- utils::modifyList(args, legend)
    do.call(graphics::legend, args = args)
  }

  invisible(x)
}

#' @export
#' @rdname plot_outlier
#' @aliases plot,OutlierIndex,missing-method
setMethod("plot", c(x = "OutlierIndex", y = "missing"), plot.OutlierIndex)
