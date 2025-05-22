# BOXPLOT
#' @include AllGenerics.R
NULL

# LogRatio =====================================================================
#' @export
#' @method boxplot LogRatio
boxplot.LogRatio <- function(x, ..., range = 1.5, width = NULL, varwidth = FALSE,
                             notch = FALSE, outline = TRUE,
                             plot = TRUE, horizontal = FALSE,
                             xlab = NULL, ylab = NULL, main = NULL, sub = NULL,
                             ann = graphics::par("ann")) {
  z <- as.data.frame(x)
  if (horizontal) {
    xlab <- xlab %||% get_transformation(x)
  } else {
    ylab <- ylab %||% get_transformation(x)
  }
  box <- graphics::boxplot(z, ..., range = range, width = width,
                           varwidth = varwidth, notch = notch, outline = outline,
                           names = labels(x), plot = plot, horizontal = horizontal,
                           xlab = xlab, ylab = ylab, main = main, sub = sub,
                           ann = ann, log = "", las = 1)
  if (!plot) return(invisible(box))

  invisible(x)
}

#' @export
#' @rdname boxplot
#' @aliases boxplot,LogRatio-method
setMethod("boxplot", c(x = "LogRatio"), boxplot.LogRatio)

# GroupedLogRatio ==============================================================
#' @export
#' @method boxplot GroupedLogRatio
boxplot.GroupedLogRatio <- function(x, ..., range = 1.5, width = NULL,
                                    varwidth = FALSE, notch = FALSE,
                                    outline = TRUE, plot = TRUE,
                                    horizontal = FALSE, color = NULL,
                                    xlab = NULL, ylab = NULL,
                                    main = NULL, sub = NULL,
                                    ann = graphics::par("ann"),
                                    legend = list(x = "topright")) {
  ## Graphical parameters
  lvl <- group_levels(x)
  col <- khroma::palette_color_discrete(color)(lvl)
  bg <- grDevices::adjustcolor(col, alpha.f = 0.5)
  cex.axis <- list(...)$cex.axis %||% graphics::par("cex.axis")
  col.axis <- list(...)$col.axis %||% graphics::par("col.axis")
  font.axis <- list(...)$font.axis %||% graphics::par("font.axis")

  ## Prepare data
  df <- data.frame(
    x = as.numeric(x),
    y = interaction(
      factor(rep(labels(x), each = nrow(x)), levels = labels(x)),
      factor(rep(group_names(x), times = ncol(x)), levels = group_levels(x)),
      lex.order = TRUE
    )
  )

  ## Plot
  box <- graphics::boxplot(
    formula = x ~ y, data = df, ...,
    range = range,
    width = width,
    varwidth = varwidth,
    notch = notch,
    outline = outline,
    plot = plot,
    border = col,
    col = bg,
    horizontal = horizontal,
    log = "",
    ann = FALSE,
    xaxt = ifelse(!horizontal, "n", "s"),
    yaxt = ifelse(horizontal, "n", "s"),
    las = 1
  )
  if (!plot) return(invisible(box))

  ## Add annotation
  if (ann) {
    if (horizontal) {
      xlab <- xlab %||% get_transformation(x)
    } else {
      ylab <- ylab %||% get_transformation(x)
    }
    graphics::title(main = main, sub = sub, xlab = xlab, ylab = ylab)
  }

  ## Construct axis
  n_group <- length(group_levels(x))
  graphics::axis(
    side = ifelse(horizontal, 2, 1),
    at = seq(from = (n_group + 1) / 2, to = ncol(x) * n_group, by = n_group),
    labels = labels(x),
    cex.axis = cex.axis,
    col.axis = col.axis,
    font.axis = font.axis,
    las = 1
  )

  ## Add legend
  if (is.list(legend) && length(legend) > 0) {
    args <- list(x = "topright", legend = lvl, fill = col, bty = "n")
    args <- utils::modifyList(args, legend)
    do.call(graphics::legend, args = args)
  }

  invisible(x)
}

#' @export
#' @rdname boxplot
#' @aliases boxplot,GroupedLogRatio-method
setMethod("boxplot", c(x = "GroupedLogRatio"), boxplot.GroupedLogRatio)
