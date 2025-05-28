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
    ## Variables
    msg_parts <- describe_coda(x)

    ## Missing values
    msg_miss <- describe_missing(x)

    ## Check
    msg_val <- describe_check(x)

    cat(msg_parts, msg_miss, msg_val, sep = "\n")

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
    ## Variables
    msg_parts <- describe_coda(x)

    ## Groups
    msg_group <- describe_groups(x)

    ## Missing values
    msg_miss <- describe_missing(x)

    ## Check
    msg_val <- describe_check(x)

    cat(paste0(msg_parts, msg_group), msg_miss, msg_val, sep = "\n")

    invisible(x)
  }
)

describe_coda <- function(x) {
  m <- nrow(x)
  p <- ncol(x)

  rows <- sprintf(ngettext(m, "%d composition", "%d compositions"), m)
  title <- sprintf("%s:", rows)

  cols <- paste0(dQuote(labels(x)), collapse = ", ")
  msg <- sprintf(ngettext(p, "%d part", "%d parts"), p)
  msg <- sprintf("\n* %s: %s.", msg, cols)

  paste0(title, msg, collapse = "")
}
describe_groups <- function(x) {

  i <- group_n(x)
  ls_grp <- paste0(dQuote(group_levels(x)), collapse = ", ")
  msg_grp <- sprintf(ngettext(i, "%d group", "%d groups"), i)
  msg_grp <- sprintf("%s: %s", msg_grp, ls_grp)

  j <- sum(!is_assigned(x))
  msg_ung <- sprintf(ngettext(j, "%d unassigned sample", "%d unassigned samples"), j)

  paste0(sprintf("\n* %s.", c(msg_grp, msg_ung)), collapse = "")
}
describe_missing <- function(x) {
  m <- nrow(x)
  p <- ncol(x)

  n_NA <- sum(count(x, f = is.na))
  m_NA <- sum(detect(x, f = is.na, margin = 1))
  p_NA <- sum(detect(x, f = is.na, margin = 2))
  pc <- label_percent(c(m_NA / m, p_NA / p), digits = 1, trim = TRUE)

  title <- sprintf(ngettext(n_NA, "%d missing value:", "%d missing values:"), n_NA)

  rows_NA <- ngettext(m_NA, "%d observation (%s) contains missing values",
                      "%d observations (%s) contain missing values")
  msg_row_NA <- sprintf(rows_NA, m_NA, pc[[1]])

  cols_NA <- ngettext(p_NA, "%d variable (%s) contains missing values",
                      "%d variables (%s) contain missing values")
  msg_col_NA <- sprintf(cols_NA, p_NA, pc[[2]])

  msg <- paste0(sprintf("\n* %s.", c(msg_row_NA, msg_col_NA)), collapse = "")
  paste0("\n", title, msg, collapse = "")
}
describe_check <- function(x) {
  title <- tr_("Data checking:")

  ## Constant columns
  p_var <- sum(detect(x, f = function(x) is_unique(x), margin = 2))
  cols_var <- ngettext(p_var, "%d variable with no variance",
                       "%d variables with no variance")
  msg_col_var <- sprintf(cols_var, p_var)

  ## Sparsity
  spa <- sparsity(x, count = FALSE)
  msg_spa <- sprintf(tr_("%s of numeric values are zero"), label_percent(spa, digits = 1))

  msg <- paste0(sprintf("\n* %s.", c(msg_spa, msg_col_var)), collapse = "")
  paste0("\n", title, msg, collapse = "")
}
