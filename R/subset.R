# ACCESSORS
#' @include AllClasses.R AllGenerics.R
NULL

# Extract ======================================================================
## [ ---------------------------------------------------------------------------
#' @export
#' @rdname subset
#' @aliases [,CompositionMatrix-method
setMethod(
  f = "[",
  signature = c(x = "CompositionMatrix"),
  function(x, i, j, ..., drop = TRUE) {
    z <- methods::callNextMethod()

    if (is.null(dim(z))) return(z)

    if (!missing(i)) {
      samples <- x@samples
      groups <- x@groups
      totals <- x@totals
      if (!is_empty(samples)) samples <- samples[i]
      if (!is_empty(groups)) groups <- groups[i]
      if (!is_empty(totals)) totals <- totals[i]
      methods::initialize(x, z, samples = samples, groups = groups,
                          totals = totals)
    } else{
      methods::initialize(x, z)
    }
  }
)

# Replace ======================================================================
## [<- -------------------------------------------------------------------------
#' @export
#' @rdname subset
#' @aliases [<-,CompositionMatrix-method
setMethod(
  f = "[<-",
  signature = c(x = "CompositionMatrix"),
  function(x, i, j, ..., value) {
    z <- methods::callNextMethod()
    methods::validObject(z)
    z
  }
)

## [[<- ------------------------------------------------------------------------
#' @export
#' @rdname subset
#' @aliases [[<-,CompositionMatrix-method
setMethod(
  f = "[[<-",
  signature = c(x = "CompositionMatrix"),
  function(x, i, j, ..., value) {
    z <- methods::callNextMethod()
    methods::validObject(z)
    z
  }
)
