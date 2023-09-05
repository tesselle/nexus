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

# Groups =======================================================================
#' @export
#' @rdname groups
#' @aliases has_groups,CompositionMatrix-method
setMethod("has_groups", "CompositionMatrix", function(x) !all(is.na(x@groups)))

#' @export
#' @rdname groups
#' @aliases has_groups,LogRatio-method
setMethod("has_groups", "LogRatio", function(x) !all(is.na(x@groups)))

#' @export
#' @rdname groups
#' @aliases has_groups,OutlierIndex-method
setMethod("has_groups", "OutlierIndex", function(x) !all(is.na(x@groups)))

#' @export
#' @rdname groups
#' @aliases get_groups,CompositionMatrix-method
setMethod("get_groups", "CompositionMatrix", function(x) x@groups)

#' @export
#' @rdname groups
#' @aliases get_groups,LogRatio-method
setMethod("get_groups", "LogRatio", function(x) x@groups)

#' @export
#' @rdname groups
#' @aliases get_groups,OutlierIndex-method
setMethod("get_groups", "OutlierIndex", function(x) x@groups)

#' @export
#' @rdname groups
#' @aliases set_groups,CompositionMatrix-method
setMethod(
  f = "set_groups<-",
  signature = "CompositionMatrix",
  definition = function(x, value) {
    empty <- rep(NA_character_, nrow(x))
    x@groups <- if (is.null(value)) empty else as.character(value)
    methods::validObject(x)
    x
  }
)

# Samples ======================================================================
#' @export
#' @rdname samples
#' @aliases has_replicates,CompositionMatrix-method
setMethod("has_replicates", "CompositionMatrix", function(x) arkhe::has_duplicates(x@samples))

#' @export
#' @rdname samples
#' @aliases has_replicates,LogRatio-method
setMethod("has_replicates", "LogRatio", function(x) arkhe::has_duplicates(x@samples))

#' @export
#' @rdname samples
#' @aliases has_replicates,OutlierIndex-method
setMethod("has_replicates", "OutlierIndex", function(x) arkhe::has_duplicates(x@samples))

#' @export
#' @rdname samples
#' @aliases get_samples,CompositionMatrix-method
setMethod("get_samples", "CompositionMatrix", function(x) x@samples)

#' @export
#' @rdname samples
#' @aliases get_samples,LogRatio-method
setMethod("get_samples", "LogRatio", function(x) x@samples)

#' @export
#' @rdname samples
#' @aliases get_samples,OutlierIndex-method
setMethod("get_samples", "OutlierIndex", function(x) x@samples)

#' @export
#' @rdname samples
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

# Totals =======================================================================
#' @export
#' @rdname totals
#' @aliases get_totals,CompositionMatrix-method
setMethod("get_totals", "CompositionMatrix", function(x) x@totals)

#' @export
#' @rdname totals
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
