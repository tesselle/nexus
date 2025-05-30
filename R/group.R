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
#' @param x A ([`list`] of) [`factor`]s for which interaction is to be computed.
#' @param drop_levels A [`logical`] scalar: should unused factor levels be
#'  dropped?
#' @param allow_na A [`logical`] scalar: should `NA` be considered an extra
#'  level?
#' @return A [`factor`] vector.
#' @keywords internal
#' @noRd
compute_groups <- function(x, drop_levels = TRUE, allow_na = TRUE) {
  if (is.list(x)) {
    x <- if (length(x) > 1) interaction(x, sep = "_") else x[[1L]]
  }
  x <- as.factor(x)
  if (drop_levels) x <- droplevels(x)
  if (allow_na) x <- addNA(x, ifany = TRUE)

  x
}

#' Validate Groups
#'
#' @param object A [`matrix`]-like object.
#' @param by A ([`list`] of) [`factor`]s for which interaction is to be computed.
#' @param verbose A [`logical`] scalar: should \R report extra information
#'  on progress?
#' @return Invisibly returns `by`.
#' @keywords internal
#' @noRd
validate_groups <- function(object, by, verbose = getOption("nexus.verbose")) {
  arkhe::assert_type(by, "integer")
  arkhe::assert_length(by, nrow(object))

  if (nlevels(by) == 0) {
    stop(tr_("Nothing to group by."), call. = FALSE)
  }
  if (isTRUE(verbose)) {
    if (nlevels(by) == nrow(object)) {
      message(tr_("As many groups as individuals."))
    }

    n <- nlevels(by)
    what <- ngettext(n, "Found %g group (%s)", "Found %g groups (%s)")
    grp <- paste0(levels(by), collapse = ", ")
    message(sprintf(what, n, grp))
  }

  invisible(by)
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
    validate_groups(object, by, verbose = verbose)

    .GroupedComposition(
      object,
      group_indices = as.integer(by),
      group_levels = levels(by),
      group_ordered = is.ordered(by)
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
      by <- c(list(group_factor(object, exclude = NULL)), by)
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
#' @rdname group_names
#' @aliases group_levels,ReferenceGroups-method
setMethod(
  f = "group_levels",
  signature = "ReferenceGroups",
  definition = function(object) object@group_levels
)

#' @export
#' @rdname group_names
#' @aliases group_names,ReferenceGroups-method
setMethod(
  f = "group_names",
  signature = "ReferenceGroups",
  definition = function(object) group_levels(object)[group_indices(object)]
)

is_ordered <- function(object) {
  isTRUE(object@group_ordered)
}

#' @export
#' @rdname group_names
#' @aliases group_factor,ReferenceGroups-method
setMethod(
  f = "group_factor",
  signature = "ReferenceGroups",
  definition = function(object, exclude = NA) {
    factor(
      x = group_names(object),
      levels = group_levels(object),
      exclude = exclude,
      ordered = is_ordered(object)
    )
  }
)

#' @export
#' @rdname group_names
#' @aliases group_indices,ReferenceGroups-method
setMethod(
  f = "group_indices",
  signature = "ReferenceGroups",
  definition = function(object) object@group_indices
)

#' @export
#' @rdname group_names
#' @aliases group_rows,ReferenceGroups-method
setMethod(
  f = "group_rows",
  signature = "ReferenceGroups",
  definition = function(object) {
    i <- group_factor(object, exclude = NULL)
    split(seq_along(i), f = i)
  }
)

#' @export
#' @rdname group_names
#' @aliases group_n,ReferenceGroups-method
setMethod(
  f = "group_n",
  signature = "ReferenceGroups",
  definition = function(object) length(group_levels(object))
)

#' @export
#' @rdname group_names
#' @aliases group_size,ReferenceGroups-method
setMethod(
  f = "group_size",
  signature = "ReferenceGroups",
  definition = function(object) lengths(group_rows(object))
)

# Predicates ===================================================================
#' @export
#' @rdname is_assigned
#' @aliases is_assigned,ReferenceGroups-method
setMethod(
  f = "is_assigned",
  signature = "ReferenceGroups",
  definition = function(object) !is.na(group_names(object))
)

#' @export
#' @rdname is_assigned
#' @aliases any_assigned,ReferenceGroups-method
setMethod(
  f = "any_assigned",
  signature = "ReferenceGroups",
  definition = function(object) any(is_assigned(object))
)

#' @export
#' @rdname is_assigned
#' @aliases all_assigned,ReferenceGroups-method
setMethod(
  f = "all_assigned",
  signature = "ReferenceGroups",
  definition = function(object) all(is_assigned(object))
)
