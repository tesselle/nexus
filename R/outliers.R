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
#' @method autoplot OutlierIndex
autoplot.OutlierIndex <- function(object, ..., qq = TRUE, limit = !qq) {
  ## Prepare data
  data <- as.data.frame(object)

  gg_qqline <- NULL
  if (qq) {
    df <- object@df
    data <- data[order(data$distance), ]
    data$quantiles <- stats::qchisq(stats::ppoints(nrow(data)), df = df)

    gg_qqline <- list(
      ggplot2::geom_qq_line(
        mapping = ggplot2::aes(sample = .data$distance),
        distribution = stats::qchisq, dparams = df,
        inherit.aes = FALSE
      ),
      ggplot2::coord_fixed()
    )
  }

  gg_limit <- NULL
  if (limit) {
    gg_limit <- ggplot2::geom_hline(yintercept = object@limit, linetype = 2)
  }

  gg_facet <- NULL
  if (has_groups(object)) {
    gg_facet <- ggplot2::facet_wrap(~ .data$group, scales = "free_x")
  }

  xval <- ifelse(qq, "quantiles", "index")
  xlab <- ifelse(qq, "Theoretrical Quantiles", "Index")
  ylab <- sprintf("%s Mahalanobis distance",
                  ifelse(object@robust, "Robust", "Standard"))

  ggplot2::ggplot(data = data) +
    ggplot2::aes(x = .data[[xval]], y = .data$distance,
                 colour = .data$outlier, shape = .data$outlier,
                 label = .data$sample) +
    gg_facet +
    gg_qqline +
    ggplot2::geom_point() +
    gg_limit +
    ggplot2::scale_x_continuous(name = xlab) +
    ggplot2::scale_y_continuous(name = ylab)
}

#' @export
#' @rdname plot_outliers
#' @aliases autoplot,OutlierIndex-method
setMethod("autoplot", "OutlierIndex", autoplot.OutlierIndex)

#' @export
#' @method plot OutlierIndex
plot.OutlierIndex <- function(x, qq = TRUE, limit = !qq, ...) {
  gg <- autoplot(object = x, qq = qq, limit = limit) +
    ggplot2::scale_colour_manual(values = c("#004488", "#DDAA33")) +
    ggplot2::theme_bw()
  print(gg)
  invisible(x)
}

#' @export
#' @rdname plot_outliers
#' @aliases plot,OutlierIndex,missing-method
setMethod("plot", c(x = "OutlierIndex", y = "missing"), plot.OutlierIndex)
