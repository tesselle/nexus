# PLOT
#' @include AllClasses.R AllGenerics.R
NULL

# CompositionMatrix ============================================================
#' @export
#' @method autoplot CompositionMatrix
autoplot.CompositionMatrix <- function(object, ..., order = NULL,
                                       decreasing = FALSE, facet = TRUE) {
  ## Prepare data
  coda <- arkhe::as_long(object, factor = TRUE)
  colnames(coda) <- c("Sample", "Part", "Value")

  ## Reorder levels
  if (!is.null(order)) {
    ordering <- order(object[, order], decreasing = decreasing)
    coda$Sample <- factor(coda$Sample, levels = levels(coda$Sample)[ordering])
  }

  ## Facet
  gg_facet <- NULL
  if (facet && has_groups(object)) {
    coda$Group <- get_groups(object)
    gg_facet <- ggplot2::facet_wrap(
      facets = ggplot2::vars(.data$Group),
      nrow = length(unique(get_groups(object))),
      scale = "free_y"
    )
  }

  ## ggplot
  ggplot2::ggplot(data = coda) +
    ggplot2::aes(x = .data$Sample, y = .data$Value, fill = .data$Part) +
    ggplot2::geom_col(position = "stack") +
    ggplot2::coord_flip() +
    gg_facet
}

#' @export
#' @rdname plot
#' @aliases autoplot,CompositionMatrix-method
setMethod("autoplot", "CompositionMatrix", autoplot.CompositionMatrix)

#' @export
#' @method plot CompositionMatrix
plot.CompositionMatrix <- function(x, order = 1, decreasing = FALSE, ...) {
  gg <- autoplot(object = x, order = order, decreasing = decreasing) +
    ggplot2::theme_bw()
  print(gg)
  invisible(x)
}

#' @export
#' @rdname plot
#' @aliases plot,CompositionMatrix,missing-method
setMethod("plot", c(x = "CompositionMatrix", y = "missing"), plot.CompositionMatrix)
