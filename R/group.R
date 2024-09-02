# GROUPS
#' @include AllGenerics.R
NULL

#' @export
#' @rdname extract
#' @aliases extract,CompositionMatrix-method
setMethod(
  f = "extract",
  signature = c("CompositionMatrix"),
  definition = function(object, name) {
    ## Validation
    arkhe::assert_type(name, "character")
    if (!any_assigned(object)) stop("No group is defined.", call. = FALSE)

    ok <- groups(object) %in% name
    if (!any(ok)) {
      g <- ngettext(length(name), "group", "groups")
      msg <- "No sample belongs to the %s %s."
      message(sprintf(msg, g, paste0(dQuote(name), collapse = ", ")))
      return(object)
    }

    object[ok, , drop = FALSE]
  }
)

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
#' @rdname groups
#' @aliases is_assigned,CompositionMatrix-method
setMethod("is_assigned", "CompositionMatrix", function(object) in_groups(groups(object)))

#' @export
#' @rdname groups
#' @aliases is_assigned,LogRatio-method
setMethod("is_assigned", "LogRatio", function(object) in_groups(groups(object)))

#' @export
#' @rdname groups
#' @aliases any_assigned,CompositionMatrix-method
setMethod("any_assigned", "CompositionMatrix", function(object) any(is_assigned(object)))

#' @export
#' @rdname groups
#' @aliases any_assigned,LogRatio-method
setMethod("any_assigned", "LogRatio", function(object) any(is_assigned(object)))

#' @export
#' @rdname groups
#' @aliases groups,CompositionMatrix-method
setMethod("groups", "CompositionMatrix", function(object) object@groups)

#' @export
#' @rdname groups
#' @aliases groups,LogRatio-method
setMethod("groups", "LogRatio", function(object) object@groups)

#' @export
#' @rdname groups
#' @aliases groups,OutlierIndex-method
setMethod("groups", "OutlierIndex", function(object) object@groups)

#' @export
#' @rdname groups
#' @aliases groups,CompositionMatrix,ANY-method
setMethod(
  f = "groups<-",
  signature = c(object = "CompositionMatrix", value = "ANY"),
  definition = function(object, value) {
    if (is.null(value)) {
      object@groups <- rep(NA_character_, nrow(object))
    } else {
      value <- as.character(value)
      value[value == ""] <- NA_character_
      object@groups <- value
    }
    methods::validObject(object)
    object
  }
)
