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
  definition = function(from, parts = NULL,
                        verbose = getOption("nexus.verbose")) {
    ## Clean row/column names
    lab <- make_names(x = NULL, n = nrow(from), prefix = "S")
    rownames(from) <- if (has_rownames(from)) rownames(from) else lab
    colnames(from) <- make_names(x = colnames(from), n = ncol(from), prefix = "V")

    ## Remove non-numeric columns
    if (is.null(parts)) {
      parts <- arkhe::detect(from, f = is.double, margin = 2) # Logical
      if (verbose) {
        n <- sum(parts)
        what <- ngettext(n, "part", "parts")
        cols <- paste0(colnames(from)[parts], collapse = ", ")
        msg <- "Found %g %s (%s)."
        message(sprintf(msg, n, what, cols))
      }
    } else {
      if (is.numeric(parts)) parts <- seq_len(ncol(from)) %in% parts
      if (is.character(parts)) parts <- colnames(from) %in% parts
    }
    coda <- from[, parts, drop = FALSE]
    extra <- from[, !parts, drop = FALSE]
    arkhe::assert_filled(coda)

    ## Build matrix
    coda <- data.matrix(coda, rownames.force = NA)
    totals <- rowSums(coda, na.rm = TRUE)
    coda <- coda / totals

    .CompositionMatrix(coda, totals = unname(totals), extra = as.list(extra))
  }
)

# To Amounts ===================================================================
#' @export
#' @rdname as_amounts
#' @aliases as_amounts,CompositionMatrix-method
setMethod(
  f = "as_amounts",
  signature = c(from = "CompositionMatrix"),
  definition = function(from) {
    methods::as(from, "matrix") * get_totals(from)
  }
)

# To Features ==================================================================
#' @export
#' @rdname as_features
#' @aliases as_features,CompositionMatrix-method
setMethod(
  f = "as_features",
  signature = c(from = "CompositionMatrix"),
  definition = function(from) {
    if (has_extra(from)) {
      data.frame(get_extra(from), from)
    } else {
      as.data.frame(from)
    }
  }
)

#' @export
#' @rdname as_features
#' @aliases as_features,LogRatio-method
setMethod(
  f = "as_features",
  signature = c(from = "LogRatio"),
  definition = function(from) {
    if (has_extra(from)) {
      data.frame(get_extra(from), from)
    } else {
      as.data.frame(from)
    }
  }
)

# To data.frame ================================================================
#' @method as.data.frame CompositionMatrix
#' @export
as.data.frame.CompositionMatrix <- function(x, ...) {
  as.data.frame(methods::as(x, "matrix"), row.names = rownames(x))
}

#' @method as.data.frame LogRatio
#' @export
as.data.frame.LogRatio <- function(x, ...) {
  as.data.frame(methods::as(x, "matrix"), row.names = rownames(x))
}

#' @method as.data.frame OutlierIndex
#' @export
as.data.frame.OutlierIndex <- function(x, ...) {
  as.data.frame(x@standard, row.names = rownames(x))
}
