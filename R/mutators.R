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

# Groups =======================================================================
has_groups <- function(x) {
  length(x) > 0 && any(in_groups(x))
}
in_groups <- function(x) {
  !is.na(x) & nzchar(x, keepNA = TRUE)
}
ngroups <- function(x) {
  length(unique(x))
}

#' @export
#' @rdname group
#' @aliases is_assigned,CompositionMatrix-method
setMethod("is_assigned", "CompositionMatrix", function(x) in_groups(group(x)))

#' @export
#' @rdname group
#' @aliases is_assigned,LogRatio-method
setMethod("is_assigned", "LogRatio", function(x) in_groups(group(x)))

#' @export
#' @rdname group
#' @aliases any_assigned,CompositionMatrix-method
setMethod("any_assigned", "CompositionMatrix", function(x) any(is_assigned(x)))

#' @export
#' @rdname group
#' @aliases any_assigned,LogRatio-method
setMethod("any_assigned", "LogRatio", function(x) any(is_assigned(x)))

#' @export
#' @rdname group
#' @aliases group,CompositionMatrix-method
setMethod("group", "CompositionMatrix", function(x) x@groups)

#' @export
#' @rdname group
#' @aliases group,LogRatio-method
setMethod("group", "LogRatio", function(x) x@groups)

#' @export
#' @rdname group
#' @aliases group,OutlierIndex-method
setMethod("group", "OutlierIndex", function(x) x@groups)

#' @export
#' @rdname group
#' @aliases group,CompositionMatrix-method
setMethod(
  f = "group<-",
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
#' @rdname total
#' @aliases total,CompositionMatrix-method
setMethod("total", "CompositionMatrix", function(x) x@totals)

#' @export
#' @rdname total
#' @aliases total,LogRatio-method
setMethod("total", "LogRatio", function(x) x@totals)

#' @export
#' @rdname total
setMethod(
  f = "total<-",
  signature = "CompositionMatrix",
  definition = function(x, value) {
    x@totals <- if (is.null(value)) rowSums(x) else as.numeric(value)
    methods::validObject(x)
    x
  }
)
