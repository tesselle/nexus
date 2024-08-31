# MUTATORS
#' @include AllGenerics.R
NULL

# Not exported
get_transformation <- function(x) {
  switch(
    class(x),
    LR = "Pairwise Log-Ratio",
    CLR = "Centered Log-Ratio",
    ALR = "Additive Log-Ratio",
    ILR = "Isometric Log-Ratio",
    PLR = "Pivot Log-Ratio"
  )
}

# Getter =======================================================================
#' @export
#' @method labels CompositionMatrix
labels.CompositionMatrix <- function(object, ...) {
  colnames(object)
}

#' @export
#' @rdname mutators
#' @aliases labels,CompositionMatrix-method
setMethod("labels", "CompositionMatrix", labels.CompositionMatrix)

#' @export
#' @method labels LogRatio
labels.LogRatio <- function(object, ...) {
  object@ratio
}

#' @export
#' @rdname mutators
#' @aliases labels,LogRatio-method
setMethod("labels", "LogRatio", labels.LogRatio)

#' @export
#' @method weights LogRatio
weights.LogRatio <- function(object, ...) {
  object@weights
}

#' @export
#' @rdname mutators
#' @aliases weights,LogRatio-method
setMethod("weights", "LogRatio", weights.LogRatio)

# Totals =======================================================================
#' @export
#' @rdname total
#' @aliases total,CompositionMatrix-method
setMethod("total", "CompositionMatrix", function(object) object@totals)

#' @export
#' @rdname total
#' @aliases total,LogRatio-method
setMethod("total", "LogRatio", function(object) object@totals)

#' @export
#' @rdname total
setMethod(
  f = "total<-",
  signature = "CompositionMatrix",
  definition = function(object, value) {
    object@totals <- if (is.null(value)) rowSums(object) else as.numeric(value)
    methods::validObject(object)
    object
  }
)
