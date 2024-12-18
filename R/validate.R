# CLASS VALIDATION
#' @include AllClasses.R
NULL

# ReferenceGroups ==============================================================
# setValidity(
#   Class = "ReferenceGroups",
#   method = function(object) {
#     ## Get data
#     group_indices <- object@group_indices
#     group_levels <- object@group_levels
#
#     cnd <- list(
#       arkhe::validate()
#     )
#     arkhe::check_class(object, cnd)
#   }
# )

# NumericMatrix ================================================================
setValidity(
  Class = "NumericMatrix",
  method = function(object) {
    cnd <- list(arkhe::validate(arkhe::assert_type(object, "numeric")))
    arkhe::check_class(object, cnd)
  }
)

# CompositionMatrix ============================================================
setValidity(
  Class = "CompositionMatrix",
  method = function(object) {
    ## Get data
    n <- nrow(object)
    totals <- object@totals

    cnd <- list(
      arkhe::validate(arkhe::assert_length(totals, n)),
      arkhe::validate(arkhe::assert_positive(object, strict = FALSE, na.rm = TRUE))
    )
    arkhe::check_class(object, cnd)
  }
)

# LogRatio =====================================================================
setValidity(
  Class = "LogRatio",
  method = function(object) {
    ## Get data
    parts <- object@parts
    ratio <- object@ratio
    order <- object@order
    base <- object@base
    weights <- object@weights

    totals <- object@totals

    n <- nrow(object)
    m <- length(parts)

    cnd <- list(
      arkhe::validate(arkhe::assert_missing(object)),
      arkhe::validate(arkhe::assert_infinite(object)),
      arkhe::validate(arkhe::assert_length(totals, n)),
      arkhe::validate(arkhe::assert_length(order, m))
    )
    arkhe::check_class(object, cnd)
  }
)
