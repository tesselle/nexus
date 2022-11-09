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
    samples <- object@samples
    groups <- object@groups
    totals <- object@totals

    cnd <- list(
      arkhe::validate(arkhe::assert_length(samples, n, empty = FALSE)),
      arkhe::validate(arkhe::assert_length(groups, n, empty = TRUE)),
      arkhe::validate(arkhe::assert_length(totals, n, empty = FALSE)),
      arkhe::validate(arkhe::assert_positive(object, strict = FALSE))
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

    samples <- object@samples
    groups <- object@groups
    totals <- object@totals

    i <- length(parts)
    j <- ncol(base)
    n <- nrow(object)

    cnd <- list(
      # arkhe::validate(arkhe::assert_length(ratio, j, empty = FALSE)),
      arkhe::validate(arkhe::assert_length(order, i, empty = FALSE)),
      arkhe::validate(arkhe::assert_length(samples, n, empty = FALSE)),
      arkhe::validate(arkhe::assert_length(groups, n, empty = TRUE)),
      arkhe::validate(arkhe::assert_length(totals, n, empty = FALSE))
    )
    arkhe::check_class(object, cnd)
  }
)
