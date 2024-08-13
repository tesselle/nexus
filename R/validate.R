# CLASS VALIDATION
#' @include AllClasses.R
NULL

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
    groups <- object@groups

    cnd <- list(
      arkhe::validate(arkhe::assert_length(totals, n, empty = FALSE)),
      arkhe::validate(arkhe::assert_length(groups, n, empty = FALSE)),
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
    groups <- object@groups

    n <- nrow(object)
    m <- length(parts)

    cnd <- list(
      arkhe::validate(arkhe::assert_missing(object)),
      arkhe::validate(arkhe::assert_infinite(object)),
      arkhe::validate(arkhe::assert_length(totals, n, empty = FALSE)),
      arkhe::validate(arkhe::assert_length(groups, n, empty = FALSE)),
      arkhe::validate(arkhe::assert_length(order, m, empty = FALSE))
    )
    arkhe::check_class(object, cnd)
  }
)
