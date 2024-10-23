# BARPLOT
#' @include AllGenerics.R
NULL

# CompositionMatrix ============================================================
#' @export
#' @method barplot CompositionMatrix
barplot.CompositionMatrix <- function(height, ...,
                                      by = groups(height),
                                      order_columns = FALSE, order_rows = NULL,
                                      decreasing = TRUE,
                                      space = 0.2, offset = 0.025,
                                      color = palette_color_discrete(),
                                      border = NA, axes = TRUE, legend = TRUE) {
  ## Validation
  if (ncol(height) < 2) {
    stop("At least two compositional parts are needed.", call. = FALSE)
  }

  ## Prepare data
  xy <- prepare_barplot(height, groups = by, order_columns = order_columns,
                        order_rows = order_rows, decreasing = decreasing,
                        offset = offset)
  parts <- factor(xy$data$column, levels = colnames(height))
  col <- color(parts)
  n <- nrow(height)

  ## Graphical parameters
  cex.axis <- list(...)$cex.axis %||% graphics::par("cex.axis")
  col.axis <- list(...)$col.axis %||% graphics::par("col.axis")
  font.axis <- list(...)$font.axis %||% graphics::par("font.axis")

  ## Save and restore
  mfrow <- graphics::par("mfrow")
  mar <- graphics::par("mar")
  mar[1] <- 3
  mar[2] <- width2line(rownames(height), cex = cex.axis) + 0.1
  mar[3] <- height2line("M", cex = cex.axis)
  mar[4] <- height2line("M", cex = cex.axis)

  old_par <- graphics::par(mfrow = mfrow, mar = mar)
  on.exit(graphics::par(old_par))

  ## Open new window
  grDevices::dev.hold()
  on.exit(grDevices::dev.flush(), add = TRUE)
  graphics::plot.new()

  ## Set plotting coordinates
  space <- 1 / n * 0.5 - space / n * 0.5
  xlim <- range(0, 1)
  ylim <- range(xy$data$y)
  ylim <- ylim + c(0, 2 * (offset + space) * legend) + c(-1, 1) * space
  graphics::plot.window(xlim = xlim, ylim = ylim, xaxs = "i", yaxs = "i")

  ## Plot
  graphics::rect(
    xleft = xy$data$xmin,
    ybottom = xy$data$y - space,
    xright = xy$data$xmax,
    ytop = xy$data$y + space,
    col = col,
    border = border
  )

  ## Construct axis
  if (axes) {
    at <- graphics::axTicks(side = 1)
    graphics::axis(side = 1, at = at, labels = label_percent(at),
                   xpd = NA, las = 1, cex.axis = cex.axis, col.axis = col.axis,
                   font.axis = font.axis)
    graphics::axis(side = 2, at = unique(xy$data$y), labels = unique(xy$data$row),
                   las = 2, lty = 0, cex.axis = cex.axis, col.axis = col.axis,
                   font.axis = font.axis)
    graphics::mtext(text = names(xy$groups), side = 4, line = 0, at = xy$groups,
                    cex = cex.axis, col = col.axis, font = font.axis)
  }

  ## Add legend
  if (legend) {
    graphics::rect(
      xleft = cumsum(xy$mean) - xy$mean,
      ybottom = max(xy$data$y) + offset + space,
      xright = cumsum(xy$mean),
      ytop = max(xy$data$y) + 2 * (offset + space),
      col = unique(col),
      border = border
    )
    graphics::mtext(text = names(xy$mean), side = 3, line = 0,
                    at = cumsum(xy$mean) - xy$mean / 2,
                    cex = cex.axis, col = unique(col), font = font.axis)
  }

  invisible(height)
}

#' @export
#' @rdname barplot
#' @aliases barplot,CompositionMatrix-method
setMethod("barplot", c(height = "CompositionMatrix"), barplot.CompositionMatrix)

prepare_barplot <- function(x, groups = NULL,
                            order_rows = NULL, order_columns = FALSE,
                            decreasing = TRUE, offset = 0.025) {
  ## Prepare groups
  n <- nrow(x)
  grp <- as_groups(groups, drop_na = FALSE)
  if (nlevels(grp) == 0 || nlevels(grp) == n) grp <- rep("", n)

  ## Relative frequencies
  x <- x / rowSums(x)

  ## Validation
  stopifnot(methods::is(x, "CompositionMatrix"))
  arkhe::assert_length(grp, n)

  ## Row order
  if (!is.null(order_rows)) {
    j <- x[, order_rows, drop = TRUE]
    i <- order(j, decreasing = decreasing)
    x <- x[i, , drop = FALSE]
    grp <- grp[i]
  }

  ## Columns order
  center <- mean(x)
  if (order_columns) {
    col_order <- order(center, decreasing = FALSE)
    center <- center[col_order]
    x <- x[, col_order, drop = FALSE]
  }

  ## Grouping
  spl <- split(x = x, f = grp)
  z <- do.call(rbind, spl)

  ## Build a long table
  row <- row(z, as.factor = TRUE)
  col <- col(z, as.factor = TRUE)
  data <- data.frame(
    row = as.vector(row),
    column = as.vector(col),
    value = as.vector(z)
  )

  xmax <- t(apply(X = z, MARGIN = 1, FUN = cumsum))
  xmin <- xmax - z
  data$xmin <- as.vector(xmin)
  data$xmax <- as.vector(xmax)
  data$y <- as.vector(n + 1 - as.numeric(row)) / n # Reverse levels order

  ## Offset
  n_grp <- length(spl)
  n_spl <- tapply(X = grp, INDEX = grp, FUN = length)
  offset <- rev(seq_len(n_grp)) * offset - offset
  data$y <- data$y + rep(offset, n_spl)[as.numeric(row)]

  list(
    data = data,
    mean = center,
    groups = 1 - cumsum(n_spl) / n + n_spl / n * 0.5 + offset
  )
}
