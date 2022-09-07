# OUTLIERS
#' @include AllClasses.R AllGenerics.R
NULL

# Find =========================================================================
#' @export
#' @rdname outliers
#' @aliases find_outliers,CompositionMatrix-method
setMethod(
  f = "find_outliers",
  signature = signature(object = "CompositionMatrix"),
  definition = function(object, level = 0.975, robust = TRUE, alpha = 0.5) {

    distance <- sqrt(stats_mahalanobis(object, robust = robust, alpha = alpha))
    limit <- sqrt(stats::qchisq(p = level, df = ncol(object) - 1))

    .OutlierIndex(
      samples = rownames(object),
      distances = distance,
      outliers = distance > limit,
      limit = limit,
      robust = robust
    )
  }
)

# Mutators =====================================================================
#' @export
#' @rdname outliers
#' @aliases count_outliers,OutlierIndex-method
setMethod(
  f = "count_outliers",
  signature = signature(object = "OutlierIndex"),
  definition = function(object) {
    if (has_groups(object)) {
      tapply(
        X = get_outliers(object),
        INDEX = get_groups(object),
        FUN = sum
      )
    } else {
      sum(get_outliers(object))
    }
  }
)

# Plot =========================================================================
#' @export
#' @rdname outliers
#' @aliases plot_outliers,OutlierIndex,missing-method
setMethod(
  f = "plot_outliers",
  signature = signature(object = "OutlierIndex", data = "missing"),
  definition = function(object) {
    data <- as.data.frame(object)

    facet_plot <- NULL
    if (has_groups(object)) {
      facet_plot <- ggplot2::facet_wrap(~ .data$groups, scales = "free_x")
    }
    ylab <- sprintf("%s Mahalanobis distance",
                    ifelse(object@robust, "Robust", "Standard"))

    ggplot2::ggplot(data = data) +
      facet_plot +
      ggplot2::aes(x = .data$index, y = .data$distances,
                   colour = .data$outlier, shape = .data$outlier,
                   label = .data$samples) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = object@limit, linetype = 2) +
      ggplot2::scale_x_continuous(name = "Index") +
      ggplot2::scale_y_continuous(name = ylab)
  }
)

#' @export
#' @rdname outliers
#' @aliases plot_outliers,OutlierIndex,CompositionMatrix-method
setMethod(
  f = "plot_outliers",
  signature = signature(object = "OutlierIndex", data = "CompositionMatrix"),
  definition = function(object, data, select = NULL) {

    if (is.null(select)) {
      select <- seq_len(ncol(data))
    } else if (is.character(select)) {
      select <- which(colnames(data) %in% select)
    } else {
      select <- as.integer(select)
    }

    ilr <- matrix(data = NA, nrow = nrow(data), ncol = length(select))
    k <- 1
    for (i in select) {
      ilr[, k] <- transform_plr(data)[, 1]
      k <- k + 1
    }
    colnames(ilr) <- colnames(data)[select]
    ilr <- arkhe::as_long(ilr, factor = TRUE)
    ilr$distance <- object@distances
    ilr$outlier <- get_outliers(object)

    ggplot2::ggplot(data = ilr) +
      ggplot2::aes(x = 1, y = .data$value,
                   colour = .data$distance, shape = .data$outlier,
                   label = .data$row) +
      ggplot2::facet_wrap(~ .data$column, nrow = 1, scales = "free_y") +
      ggplot2::geom_jitter() +
      ggplot2::scale_y_continuous(name = "Isometric log-ratio") +
      ggplot2::theme(
        axis.title.x = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
      )
  }
)
