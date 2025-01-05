# COERCION
#' @include AllGenerics.R
NULL

# To CompositionMatrix =========================================================
#' @export
#' @rdname as_composition
#' @aliases as_composition,numeric-method
setMethod(
  f = "as_composition",
  signature = c(from = "numeric"),
  definition = function(from) {
    from <- matrix(data = from, nrow = 1, ncol = length(from))
    methods::callGeneric(from)
  }
)

#' @export
#' @rdname as_composition
#' @aliases as_composition,matrix-method
setMethod(
  f = "as_composition",
  signature = c(from = "matrix"),
  definition = function(from) {
    ## Validation
    arkhe::assert_type(from, "numeric")

    ## Make row/column names
    lab <- make_names(x = NULL, n = nrow(from), prefix = "S")
    rownames(from) <- if (has_rownames(from)) rownames(from) else lab
    colnames(from) <- make_names(x = colnames(from), n = ncol(from), prefix = "V")

    ## Close
    totals <- rowSums(from, na.rm = TRUE)
    from <- from / totals

    .CompositionMatrix(from, totals = unname(totals))
  }
)

#' @export
#' @rdname as_composition
#' @aliases as_composition,data.frame-method
setMethod(
  f = "as_composition",
  signature = c(from = "data.frame"),
  definition = function(from, parts = NULL, groups = NULL, autodetect = TRUE,
                        verbose = getOption("nexus.verbose")) {
    ## Clean row/column names
    lab <- make_names(x = NULL, n = nrow(from), prefix = "S")
    rownames(from) <- if (has_rownames(from)) rownames(from) else lab
    colnames(from) <- make_names(x = colnames(from), n = ncol(from), prefix = "V")

    ## Remove non-numeric columns
    if (is.null(parts)) {
      if (isTRUE(autodetect)) {
        parts <- arkhe::detect(from, f = is.numeric, margin = 2)
        if (isTRUE(verbose)) {
          n <- sum(parts)
          what <- ngettext(n, "Found %g part (%s)", "Found %g parts (%s)")
          cols <- paste0(colnames(from)[parts], collapse = ", ")
          message(sprintf(what, n, cols))
        }
      } else {
        arkhe::assert_filled(parts)
      }
    } else {
      if (is.numeric(parts)) parts <- seq_len(ncol(from)) %in% parts
      if (is.character(parts)) parts <- colnames(from) %in% parts
    }
    coda <- from[, parts, drop = FALSE]
    arkhe::assert_filled(coda)

    ## Build matrix
    coda <- data.matrix(coda, rownames.force = NA)
    totals <- rowSums(coda, na.rm = TRUE)
    coda <- coda / totals

    z <- .CompositionMatrix(coda, totals = unname(totals))
    if (is.null(groups)) return(z)

    ## Group names
    grp <- from[, groups, drop = FALSE]
    group(z, by = grp, verbose = verbose)
  }
)

# To amounts ===================================================================
#' @export
#' @rdname as_amounts
#' @aliases as_amounts,CompositionMatrix-method
setMethod(
  f = "as_amounts",
  signature = c(from = "CompositionMatrix"),
  definition = function(from) {
    methods::as(from, "matrix") * totals(from)
  }
)

# To data.frame ================================================================
#' @method as.data.frame CompositionMatrix
#' @export
as.data.frame.CompositionMatrix <- function(x, row.names = rownames(x),
                                            optional = FALSE, ...) {
  as.data.frame(methods::as(x, "matrix"), row.names = row.names, optional = optional)
}

#' @export
#' @rdname as.data.frame
#' @aliases as.data.frame,CompositionMatrix-method
setMethod("as.data.frame", "CompositionMatrix", as.data.frame.CompositionMatrix)

#' @method as.data.frame GroupedComposition
#' @export
as.data.frame.GroupedComposition <- function(x, row.names = rownames(x),
                                             optional = FALSE, ...,
                                             group_var = ".group") {
  z <- data.frame(
    methods::as(x, "matrix"),
    row.names = row.names,
    check.names = !optional
  )
  z[[group_var]] <- group_names(x)
  z
}

#' @export
#' @rdname as.data.frame
#' @aliases as.data.frame,GroupedComposition-method
setMethod("as.data.frame", "GroupedComposition", as.data.frame.GroupedComposition)

#' @method as.data.frame LogRatio
#' @export
as.data.frame.LogRatio <- function(x, row.names = rownames(x),
                                   optional = FALSE, ...) {
  as.data.frame(methods::as(x, "matrix"), row.names = row.names, optional = optional)
}

#' @export
#' @rdname as.data.frame
#' @aliases as.data.frame,LogRatio-method
setMethod("as.data.frame", "LogRatio", as.data.frame.LogRatio)

#' @method as.data.frame GroupedLogRatio
#' @export
as.data.frame.GroupedLogRatio <- function(x, row.names = rownames(x),
                                          optional = FALSE, ...,
                                          group_var = ".group") {
  z <- data.frame(
    methods::as(x, "matrix"),
    row.names = row.names,
    check.names = !optional
  )
  z[[group_var]] <- group_names(x)
  z
}

#' @export
#' @rdname as.data.frame
#' @aliases as.data.frame,GroupedLogRatio-method
setMethod("as.data.frame", "GroupedLogRatio", as.data.frame.GroupedLogRatio)

#' @method as.data.frame OutlierIndex
#' @export
as.data.frame.OutlierIndex <- function(x, row.names = rownames(x),
                                       optional = FALSE, ...) {
  as.data.frame(x@standard, row.names = row.names, optional = optional)
}

#' @export
#' @rdname as.data.frame
#' @aliases as.data.frame,OutlierIndex-method
setMethod("as.data.frame", "OutlierIndex", as.data.frame.OutlierIndex)
