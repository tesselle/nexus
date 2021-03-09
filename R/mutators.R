# MUTATORS
#' @include AllClasses.R
NULL

# Getters ======================================================================
#' @export
#' @rdname mutator
#' @aliases has_groups,OutlierIndex-method
setMethod("has_groups", "OutlierIndex", function(x) length(x@groups) > 0)

#' @export
#' @rdname mutator
#' @aliases get_groups,OutlierIndex-method
setMethod("get_groups", "OutlierIndex", function(x) x@groups)

#' @export
#' @rdname mutator
#' @aliases get_groups,OutlierIndex-method
setMethod("get_outliers", "OutlierIndex", function(x) {
  out <- x@outliers
  names(out) <- x@samples
  out
})
