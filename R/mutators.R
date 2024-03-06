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

get_features <- function(x) {
  cbind(
    identifier = get_identifiers(x),
    sample = get_samples(x),
    group = get_groups(x)
  )
}

has_rownames <- function(x) {
  .row_names_info(x, type = 1L) > 0L &&
    !is.na(.row_names_info(x, type = 0L)[[1L]])
}

# Groups =======================================================================
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
#' @aliases is_assigned,OutlierIndex-method
setMethod("is_assigned", "OutlierIndex", function(x) !is.na(get_groups(x)))

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
#' @aliases any_assigned,OutlierIndex-method
setMethod("any_assigned", "OutlierIndex", function(x) any(is_assigned(x)))

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
    if (is.null(value)) {
      x@groups <- empty
    } else {
      value <- as.character(value)
      value[value == ""] <- NA_character_
      x@groups <- value
    }
    methods::validObject(x)
    x
  }
)

# Samples ======================================================================
#' @export
#' @rdname samples
#' @aliases is_replicated,CompositionMatrix-method
setMethod(
  f = "is_replicated",
  signature = "CompositionMatrix",
  definition = function(x) {
    spl <- get_samples(x)
    duplicated(spl, fromLast = FALSE) | duplicated(spl, fromLast = TRUE)
  }
)

#' @export
#' @rdname samples
#' @aliases is_replicated,LogRatio-method
setMethod(
  f = "is_replicated",
  signature = "LogRatio",
  definition = function(x) {
    spl <- get_samples(x)
    duplicated(spl, fromLast = FALSE) | duplicated(spl, fromLast = TRUE)
  }
)

#' @export
#' @rdname samples
#' @aliases is_replicated,OutlierIndex-method
setMethod(
  f = "is_replicated",
  signature = "OutlierIndex",
  definition = function(x) {
    spl <- get_samples(x)
    duplicated(spl, fromLast = FALSE) | duplicated(spl, fromLast = TRUE)
  }
)

#' @export
#' @rdname samples
#' @aliases any_replicated,CompositionMatrix-method
setMethod("any_replicated", "CompositionMatrix", function(x) any(is_replicated(x)))

#' @export
#' @rdname samples
#' @aliases any_replicated,LogRatio-method
setMethod("any_replicated", "LogRatio", function(x) any(is_replicated(x)))

#' @export
#' @rdname samples
#' @aliases any_replicated,OutlierIndex-method
setMethod("any_replicated", "OutlierIndex", function(x) any(is_replicated(x)))

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
    if (is.null(value)) {
      value <- make_names(x = NULL, n = nrow(x), prefix = "S")
    } else {
      value <- as.character(value)
    }

    x@samples <- value
    methods::validObject(x)
    x
  }
)

# Identifiers ==================================================================
#' @export
#' @rdname identifiers
#' @aliases get_identifiers,CompositionMatrix-method
setMethod("get_identifiers", "CompositionMatrix", function(x) x@codes)

#' @export
#' @rdname identifiers
#' @aliases get_identifiers,LogRatio-method
setMethod("get_identifiers", "LogRatio", function(x) x@codes)

#' @export
#' @rdname identifiers
#' @aliases get_identifiers,OutlierIndex-method
setMethod("get_identifiers", "OutlierIndex", function(x) x@codes)

#' @export
#' @rdname identifiers
#' @aliases set_identifiers,CompositionMatrix-method
setMethod(
  f = "set_identifiers<-",
  signature = "CompositionMatrix",
  definition = function(x, value) {
    if (is.null(value)) {
      value <- make_codes(get_samples(x))
    } else {
      value <- make_codes(value)
    }

    x@codes <- value
    methods::validObject(x)
    rownames(x) <- value
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
