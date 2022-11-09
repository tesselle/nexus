# HELPERS

# Helpers ======================================================================
#' Helpers
#'
#' * `\%||\%` allows to define a default value.
#' @param x,y An object.
#' @references
#'  Wickham, H. (2014). *Advanced R*. London: Chapman & Hall. The R Series.
#' @family utilities
#' @keywords internal utilities
#' @noRd
`%||%` <- function(x, y) {
  if (!is.null(x) && length(x) != 0) x else y
}

missingORnull <- function(x) {
  missing(x) || is.null(x)
}
