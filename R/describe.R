# DATA SUMMARY: DESCRIBE
#' @include AllGenerics.R
NULL

#' @export
#' @rdname describe
#' @aliases describe,CompositionMatrix-method
setMethod(
  f = "describe",
  signature = c(x = "CompositionMatrix"),
  definition = function(x) {
    ## Dimensions
    m <- nrow(x)
    p <- ncol(x)
    rows <- sprintf(ngettext(m, "%d composition", "%d compositions"), m)
    cols <- sprintf(ngettext(p, "with %d part", "with %d parts"), p)
    msg_tbl <- sprintf("%s %s:", rows, cols)

    ## Message
    cat(msg_tbl)
    .describe(x)

    invisible(x)
  }
)

#' @export
#' @rdname describe
#' @aliases describe,GroupedComposition-method
setMethod(
  f = "describe",
  signature = c(x = "GroupedComposition"),
  definition = function(x) {
    ## Dimensions
    m <- nrow(x)
    p <- ncol(x)

    rows <- sprintf(ngettext(m, "%d composition", "%d compositions"), m)
    cols <- sprintf(ngettext(p, "with %d part", "with %d parts"), p)
    msg_tbl <- sprintf("%s %s:", rows, cols)

    ## Groups
    i <- group_length(x)
    ls_grp <- paste0(dQuote(group_levels(x)), collapse = ", ")
    msg_grp <- sprintf(ngettext(i, "%d group", "%d groups"), i)
    msg_grp <- sprintf("%s: %s.", msg_grp, ls_grp)

    j <- sum(!is_assigned(x))
    msg_ung <- sprintf(ngettext(j, "%d unassigned sample.", "%d unassigned samples."), j)

    cat(msg_tbl, msg_grp, msg_ung, sep = "\n* ")
    .describe(x)

    invisible(x)
  }
)

.describe <- function(x) {
  m <- nrow(x)
  p <- ncol(x)

  ## Missing values
  m_NA <- sum(arkhe::detect(x, f = is.na, margin = 1))
  p_NA <- sum(arkhe::detect(x, f = is.na, margin = 2))

  rows_NA <- ngettext(m_NA, "composition", "compositions")
  cols_NA <- ngettext(p_NA, "part", "parts")

  pc <- arkhe::label_percent(c(m_NA / m, p_NA / p), digits = 1, trim = TRUE)
  pc_NA <- sprintf(" (%s)", pc)

  msg_row_NA <- sprintf("%d %s%s containing missing values.", m_NA, rows_NA, pc_NA[[1]])
  msg_col_NA <- sprintf("%d %s%s containing missing values.", p_NA, cols_NA, pc_NA[[2]])

  ## Constant columns
  p_var <- sum(arkhe::detect(x, f = function(x) is_unique(x), margin = 2))
  cols_var <- ngettext(p_var, "%d part with no variance.", "%d parts with no variance.")
  msg_col_var <- sprintf(cols_var, p_var)

  ## Sparsity
  spa <- arkhe::sparsity(x, count = FALSE)
  msg_spa <- sprintf("%s of values are zero.", label_percent(spa, digits = 1))

  cat("\nData checking:", msg_spa, msg_col_var, sep = "\n* ")
  cat("\nMissing values:", msg_row_NA, msg_col_NA, sep = "\n* ")
}
