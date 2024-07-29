# HELPERS

missingORnull <- function(x) {
  missing(x) || is.null(x)
}

has_rownames <- function(x) {
  .row_names_info(x, type = 1L) > 0L &&
    !is.na(.row_names_info(x, type = 0L)[[1L]])
}

#' Label Percentages
#'
#' @param x A [`numeric`] vector.
#' @param digits An [`integer`] indicating the number of decimal places.
#'  If `NULL` (the default), breaks will have the minimum number of digits
#'  needed to show the difference between adjacent values.
#' @param trim A [`logical`] scalar. If `FALSE` (the default), values are
#'  right-justified to a common width (see [base::format()]).
#' @return A [`character`] vector.
#' @keywords internal
#' @noRd
label_percent <- function(x, digits = NULL, trim = FALSE) {
  i <- !is.na(x)
  y <- x[i]
  y <- abs(y) * 100
  y <- format(y, trim = trim, digits = digits)
  y <- paste0(y, "%")
  x[i] <- y
  x
}

flatten_chr <- function(x, by) {
  z <- tapply(X = x, INDEX = by, FUN = unique, simplify = FALSE)
  z <- vapply(X = z, FUN = paste0, FUN.VALUE = character(1), collapse = ":")
  z
}

is_replicated <- function(x) {
  duplicated(x, fromLast = FALSE) | duplicated(x, fromLast = TRUE)
}
any_replicated <- function(x) {
  any(is_replicated(x))
}

make_codes <- function(x) {
  if (!any(duplicated(x))) return(x)
  x <- split(x = seq_along(x), f = x)
  nm <- rep(names(x), lengths(x))
  nm <- tapply(
    X = nm,
    INDEX = nm,
    FUN = function(x) paste(x, seq_along(x), sep = "_"),
    simplify = FALSE
  )

  x <- unlist(x, use.names = FALSE)
  nm <- unlist(nm, use.names = FALSE)
  nm[x]
}

make_names <- function(x, n = length(x), prefix = "X") {
  x <- if (n > 0) x %||% paste0(prefix, seq_len(n)) else character(0)
  x <- make.unique(x, sep = "_")
  x
}
