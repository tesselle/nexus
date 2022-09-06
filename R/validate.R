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
      arkhe::validate(arkhe::assert_numeric(object, "positive", strict = FALSE,
                                            na.rm = TRUE))
    )
    arkhe::check_class(object, cnd)
  }
)
