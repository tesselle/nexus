# COERCION
#' @include AllGenerics.R AllClasses.R
NULL

# To data.frame ================================================================
#' @method as.data.frame OutlierIndex
#' @export
as.data.frame.OutlierIndex <- function(x, ..., stringsAsFactors = default.stringsAsFactors()) {
  z <- data.frame(
    index = seq_along(x@outliers),
    samples = x@samples,
    distances = x@distances,
    outlier = x@outliers,
    stringsAsFactors = stringsAsFactors
  )
  if (has_groups(x)) z$groups <- x@groups
  z
}
