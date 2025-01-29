# GROUPS
#' @include AllGenerics.R
NULL

.group_extract <- function(object, which) {
  ## Validation
  arkhe::assert_type(which, "character")
  if (!any_assigned(object)) stop("No group is defined.", call. = FALSE)

  ok <- group_names(object) %in% which
  if (!any(ok)) {
    msg <- ngettext(length(which), "No sample belongs to the group: %s.",
                    "No sample belongs to the groups: %s.")
    message(sprintf(msg, paste0(dQuote(which), collapse = ", ")))
    return(object)
  }

  object[ok, , drop = FALSE]
}

#' @export
#' @rdname group_subset
#' @aliases group_subset,GroupedComposition-method
setMethod(
  f = "group_subset",
  signature = c("GroupedComposition"),
  definition = .group_extract
)

#' @export
#' @rdname group_subset
#' @aliases group_subset,GroupedLogRatio-method
setMethod(
  f = "group_subset",
  signature = c("GroupedLogRatio"),
  definition = .group_extract
)

# Groups =======================================================================
#' Compute Groups
#'
#' @param x A ([`list`] of) [`factors`] for which interaction is to be computed.
#' @param drop_levels A [`logical`] scalar: should unused factor levels be
#'  dropped?
#' @param allow_na A [`logical`] scalar: should `NA` be considered an extra
#'  level?
#' @return A [`factor`] vector.
#' @keywords internal
#' @noRd
compute_groups <- function(x, drop_levels = TRUE, allow_na = TRUE) {
  if (!is.list(x)) x <- list(x)
  x <- interaction(x, sep = "_")
  if (drop_levels) x <- droplevels(x)
  if (allow_na) x <- addNA(x, ifany = TRUE)

  x
}

#' @export
#' @rdname group
#' @aliases group,CompositionMatrix-method
setMethod(
  f = "group",
  signature = "CompositionMatrix",
  definition = function(object, by, verbose = getOption("nexus.verbose"), ...) {
    ## Compute groups
    by <- compute_groups(by, ...)

    ## Validation
    arkhe::assert_length(by, nrow(object))
    if (nlevels(by) == 0) {
      stop("Nothing to group by.", call. = FALSE)
    }
    if (isTRUE(verbose)) {
      if (nlevels(by) == nrow(object)) {
        message("As many groups as individuals.")
      }

      n <- nlevels(by)
      what <- ngettext(n, "Found %g group (%s)", "Found %g groups (%s)")
      grp <- paste0(levels(by), collapse = ", ")
      message(sprintf(what, n, grp))
    }

    .GroupedComposition(
      object,
      group_indices = as.integer(by),
      group_levels = levels(by)
    )
  }
)

#' @export
#' @rdname group
#' @aliases group,GroupedComposition-method
setMethod(
  f = "group",
  signature = "GroupedComposition",
  definition = function(object, by, add = FALSE,
                        verbose = getOption("nexus.verbose"), ...) {
    ## Compute groups
    if (isTRUE(add)) {
      if (!is.list(by)) by <- list(by)
      by <- c(list(group_factor(object)), by)
    }
    methods::callNextMethod(object, by = by, verbose = verbose, ...)
  }
)

#' @export
#' @rdname group
#' @aliases ungroup,GroupedComposition-method
setMethod(
  f = "ungroup",
  signature = "GroupedComposition",
  definition = function(object) {
    methods::as(object, "CompositionMatrix", strict = TRUE)
  }
)

#' @export
#' @rdname group
#' @aliases ungroup,GroupedLR-method
setMethod(
  f = "ungroup",
  signature = "GroupedLR",
  definition = function(object) {
    methods::as(object, "LR", strict = TRUE)
  }
)

#' @export
#' @rdname group
#' @aliases ungroup,GroupedCLR-method
setMethod(
  f = "ungroup",
  signature = "GroupedCLR",
  definition = function(object) {
    methods::as(object, "CLR", strict = TRUE)
  }
)

#' @export
#' @rdname group
#' @aliases ungroup,GroupedALR-method
setMethod(
  f = "ungroup",
  signature = "GroupedALR",
  definition = function(object) {
    methods::as(object, "ALR", strict = TRUE)
  }
)

#' @export
#' @rdname group
#' @aliases ungroup,GroupedILR-method
setMethod(
  f = "ungroup",
  signature = "GroupedILR",
  definition = function(object) {
    methods::as(object, "ILR", strict = TRUE)
  }
)

#' @export
#' @rdname group
#' @aliases ungroup,GroupedPLR-method
setMethod(
  f = "ungroup",
  signature = "GroupedPLR",
  definition = function(object) {
    methods::as(object, "PLR", strict = TRUE)
  }
)

# Metadata =====================================================================
#' @export
#' @describeIn group_metadata returns a [`character`] vector giving the group
#'  names.
#' @aliases group_levels,ReferenceGroups-method
setMethod(
  f = "group_levels",
  signature = "ReferenceGroups",
  definition = function(object) object@group_levels
)

#' @export
#' @describeIn group_metadata returns a [`character`] vector giving the name of
#'  the group that each observation belongs to.
#' @aliases group_names,ReferenceGroups-method
setMethod(
  f = "group_names",
  signature = "ReferenceGroups",
  definition = function(object) group_levels(object)[group_indices(object)]
)

group_factor <- function(object) {
  factor(
    x = group_names(object),
    levels = group_levels(object),
    exclude = NULL
  )
}

#' @export
#' @describeIn group_metadata returns an [`integer`] vector giving the group
#'  that each value belongs to.
#' @aliases group_indices,ReferenceGroups-method
setMethod(
  f = "group_indices",
  signature = "ReferenceGroups",
  definition = function(object) object@group_indices
)

#' @export
#' @describeIn group_metadata returns a `list` of [`integer`] vectors giving the
#'  observation that each group contains.
#' @aliases group_rows,ReferenceGroups-method
setMethod(
  f = "group_rows",
  signature = "ReferenceGroups",
  definition = function(object) {
    i <- group_factor(object)
    split(seq_along(i), f = i)
  }
)

#' @export
#' @describeIn group_metadata gives the total number of groups.
#' @aliases group_length,ReferenceGroups-method
setMethod(
  f = "group_length",
  signature = "ReferenceGroups",
  definition = function(object) length(group_levels(object))
)

#' @export
#' @describeIn group_metadata gives the size of each group.
#' @aliases group_size,ReferenceGroups-method
setMethod(
  f = "group_size",
  signature = "ReferenceGroups",
  definition = function(object) lengths(group_rows(object))
)

#' @export
#' @describeIn group_metadata returns a [`logical`] vector specifying whether or
#'  not an observation belongs to a group.
#' @aliases is_assigned,ReferenceGroups-method
setMethod(
  f = "is_assigned",
  signature = "ReferenceGroups",
  definition = function(object) !is.na(group_names(object))
)

#' @export
#' @describeIn group_metadata returns an [`logical`] scalar specifying if any
#'  observation belongs to a group.
#' @aliases any_assigned,ReferenceGroups-method
setMethod(
  f = "any_assigned",
  signature = "ReferenceGroups",
  definition = function(object) any(is_assigned(object))
)

#' @export
#' @describeIn group_metadata returns an [`logical`] scalar specifying if all
#'  observations belong to a group.
#' @aliases all_assigned,ReferenceGroups-method
setMethod(
  f = "all_assigned",
  signature = "ReferenceGroups",
  definition = function(object) all(is_assigned(object))
)
