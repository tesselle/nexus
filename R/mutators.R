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

# Supplementary variables ======================================================
get_extra <- function(x) {
  x@extra
}

has_extra <- function(x) {
  extra <- get_extra(x)
  length(extra) > 0 && all(lengths(extra) > 0)
}

get_variable <- function(x, which = NULL) {
  if (is.character(which) && length(which) == 1) {
    extra <- get_extra(x)[[which]]
    if (is.null(extra)) {
      warning(sprintf("There is no such variable: %s.", which), call. = FALSE)
    }
    return(extra)
  }
  which
}

# Groups =======================================================================
# is_assigned any_assigned
#' @export
#' @rdname groups
#' @aliases get_groups,OutlierIndex-method
setMethod("get_groups", "OutlierIndex", function(x) x@groups)

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
