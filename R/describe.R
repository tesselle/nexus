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

    rows <- ngettext(m, "composition", "compositions")
    cols <- ngettext(p, "part", "parts")

    msg_tbl <- sprintf("%d %s with %d %s:", m, rows, p, cols)

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
    cols_var <- ngettext(p_var, "part", "parts")
    msg_col_var <- sprintf("%d %s with no variance.", p_var, cols_var)

    ## Sparsity
    spa <- arkhe::sparsity(x, count = FALSE)
    msg_spa <- sprintf("%s of values are zero.", label_percent(spa, digits = 1))

    ## Groups
    groups <- group(x)
    grp <- unique(groups[!is.na(groups)])
    n_grp <- length(grp)
    n_ung <- sum(is.na(groups))
    ls_grp <- if (n_grp == 0) "" else paste0(": ", paste0(dQuote(grp), collapse = ", "))
    msg_grp <- sprintf("%d %s%s.", n_grp, ngettext(n_grp, "group", "groups"),
                       ls_grp)
    msg_ung <- sprintf("%d unassigned %s.", n_ung, ngettext(n_ung, "sample", "samples"))

    cat(msg_tbl, msg_grp, msg_ung, sep = "\n* ")
    cat("\nData checking:", msg_spa, msg_col_var, sep = "\n* ")
    cat("\nMissing values:", msg_row_NA, msg_col_NA, sep = "\n* ")

    invisible(x)
  }
)
