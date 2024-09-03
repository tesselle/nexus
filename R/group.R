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
as_groups <- function(x, drop_levels = TRUE, drop_na = TRUE) {
  if (!is.factor(x)) {
    if (!is.list(x)) x <- list(x)
    x <- rapply(
      object = x,
      f = function(x) {
        x[x == ""] <- NA
        x
      },
      classes = "character",
      how = "replace"
    )
    x <- interaction(x, sep = "_")
  }
  if (drop_levels) x <- droplevels(x)
  if (!drop_na) x <- addNA(x, ifany = TRUE)
  x
}
in_groups <- function(x) {
  !is.na(x) & x != ""
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
    if (is.null(value)) value <- rep(NA_character_, nrow(object))
    value <- as_groups(value)
    object@groups <- value
    methods::validObject(object)
    object
  }
)

#' @export
#' @rdname groups
#' @aliases groups,CompositionMatrix,list-method
setMethod(
  f = "groups<-",
  signature = c(object = "CompositionMatrix", value = "list"),
  definition = function(object, value) {
    value <- as_groups(value)
    object@groups <- value
    methods::validObject(object)
    object
  }
)
