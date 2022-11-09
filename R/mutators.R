# MUTATORS
#' @include AllClasses.R AllGenerics.R
NULL

# Getters ======================================================================
#' @export
#' @rdname mutators
#' @aliases has_groups,CompositionMatrix-method
setMethod("has_groups", "CompositionMatrix", function(x) !arkhe::is_empty(x@groups))

#' @export
#' @rdname mutators
#' @aliases get_groups,CompositionMatrix-method
setMethod("get_groups", "CompositionMatrix", function(x) x@groups)

#' @export
#' @rdname mutators
#' @aliases get_samples,CompositionMatrix-method
setMethod("get_samples", "CompositionMatrix", function(x) x@samples)

#' @export
#' @rdname mutators
#' @aliases get_totals,CompositionMatrix-method
setMethod("get_totals", "CompositionMatrix", function(x) x@totals)

#' @export
#' @rdname mutators
#' @aliases has_groups,LogRatio-method
setMethod("has_groups", "LogRatio", function(x) !arkhe::is_empty(x@groups))

#' @export
#' @rdname mutators
#' @aliases get_groups,LogRatio-method
setMethod("get_groups", "LogRatio", function(x) x@groups)

#' @export
#' @rdname mutators
#' @aliases get_samples,LogRatio-method
setMethod("get_samples", "LogRatio", function(x) x@samples)

#' @export
#' @rdname mutators
#' @aliases has_groups,OutlierIndex-method
setMethod("has_groups", "OutlierIndex", function(x) !arkhe::is_empty(x@groups))

#' @export
#' @rdname mutators
#' @aliases get_groups,OutlierIndex-method
setMethod("get_groups", "OutlierIndex", function(x) x@groups)

#' @export
#' @rdname mutators
#' @aliases get_samples,OutlierIndex-method
setMethod("get_samples", "OutlierIndex", function(x) x@samples)


# Setters ======================================================================
#' @export
#' @rdname mutators
#' @aliases set_groups,CompositionMatrix-method
setMethod(
  f = "set_groups<-",
  signature = "CompositionMatrix",
  definition = function(x, value) {
    x@groups <- if (is.null(value)) character(0) else as.character(value)
    methods::validObject(x)
    x
  }
)

#' @export
#' @rdname mutators
#' @aliases set_samples,CompositionMatrix-method
setMethod(
  f = "set_samples<-",
  signature = "CompositionMatrix",
  definition = function(x, value) {
    x@samples <- if (is.null(value)) rownames(x) else as.character(value)
    methods::validObject(x)
    x
  }
)

#' @export
#' @rdname mutators
#' @aliases set_totals,CompositionMatrix-method
setMethod(
  f = "set_totals<-",
  signature = "CompositionMatrix",
  definition = function(x, value) {
    x@totals <- if (is.null(value)) rowSums(x) else as.numeric(value)
    methods::validObject(x)
    x
  }
)
