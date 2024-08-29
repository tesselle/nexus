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
#' @method weights LogRatio
weights.LogRatio <- function(object, ...) {
  object@weights
}

#' @export
#' @rdname mutators
#' @aliases weights,LogRatio-method
setMethod("weights", "LogRatio", weights.LogRatio)

# Groups =======================================================================
has_groups <- function(x) {
  length(x) > 0 && !all(is.na(x))
}

#' @export
#' @rdname groups
#' @aliases is_assigned,CompositionMatrix-method
setMethod("is_assigned", "CompositionMatrix", function(x) !is.na(get_groups(x)))

#' @export
#' @rdname groups
#' @aliases is_assigned,LogRatio-method
setMethod("is_assigned", "LogRatio", function(x) !is.na(get_groups(x)))

#' @export
#' @rdname groups
#' @aliases any_assigned,CompositionMatrix-method
setMethod("any_assigned", "CompositionMatrix", function(x) any(is_assigned(x)))

#' @export
#' @rdname groups
#' @aliases any_assigned,LogRatio-method
setMethod("any_assigned", "LogRatio", function(x) any(is_assigned(x)))

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
    if (is.null(value)) {
      x@groups <- rep(NA_character_, nrow(x))
    } else {
      value <- as.character(value)
      value[value == ""] <- NA_character_
      x@groups <- value
    }
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
#' @aliases get_totals,LogRatio-method
setMethod("get_totals", "LogRatio", function(x) x@totals)

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
