# COERCION
#' @include AllGenerics.R AllClasses.R
NULL

# To CompositionMatrix =========================================================
setAs(
  from = "matrix",
  to = "CompositionMatrix",
  def = function(from) {
    to <- data.matrix(from, rownames.force = NA)
    totals <- rowSums(to, na.rm = TRUE)
    to <- to / totals
    dimnames(to) <- make_dimnames(from)

    samples <- rownames(to) %||% character(0)
    .CompositionMatrix(to, totals = totals, samples = samples)
  }
)

setAs(
  from = "data.frame",
  to = "CompositionMatrix",
  def = function(from) {
    mtx <- data.matrix(from, rownames.force = NA)
    methods::callGeneric(from)
  }
)

#' @export
#' @rdname coerce
#' @aliases as_composition,matrix-method
setMethod(
  f = "as_composition",
  signature = signature(from = "matrix"),
  definition = function(from) {
    methods::as(from, "CompositionMatrix")
  }
)

#' @export
#' @rdname coerce
#' @aliases as_composition,data.frame-method
setMethod(
  f = "as_composition",
  signature = signature(from = "data.frame"),
  definition = function(from, samples = NULL, groups = NULL) {

    cols <- colnames(from)
    auto <- getOption("nexus.autodetect") && !is.null(cols)
    index <- function(what, where) {
      grep(what, where, ignore.case = TRUE, value = FALSE)
    }

    ## Samples
    spl <- rownames(from) %||% character(0)
    if (is.null(samples) && auto) samples <- index("^sample[s]{0,1}$", cols)
    if (arkhe::has_length(samples, 1)) spl <- as.character(from[[samples]])

    ## Groups
    grp <- character(0)
    if (is.null(groups) && auto) groups <- index("^group[s]{0,1}$", cols)
    if (arkhe::has_length(groups, 1)) grp <- as.character(from[[groups]])

    ## Drop extra columns (if any)
    drop <- c(samples, groups)
    data <- if (length(drop) > 0) from[, -drop, drop = FALSE] else from
    arkhe::assert_filled(data)

    ## Remove non-numeric columns
    ok_num <- arkhe::detect(data, arkhe::is_numeric, 2)
    data <- data[, ok_num, drop = FALSE]
    arkhe::assert_filled(data)

    ## Build matrix
    data <- data.matrix(data, rownames.force = NA)
    totals <- rowSums(data, na.rm = TRUE)
    data <- data / totals
    dimnames(data) <- make_dimnames(from)

    .CompositionMatrix(data, totals = totals, samples = spl, groups = grp)
  }
)

#' @export
#' @rdname coerce
#' @aliases as_count,CompositionMatrix-method
setMethod(
  f = "as_count",
  signature = signature(from = "CompositionMatrix"),
  definition = function(from) {
    totals <- from@totals
    counts <- from * totals

    methods::S3Part(counts, strictS3 = TRUE)
  }
)

# To data.frame ================================================================
#' @method as.data.frame CompositionMatrix
#' @export
as.data.frame.CompositionMatrix <- function(x, ...) {
  z <- as.data.frame(methods::as(x, "matrix"))

  z$samples <- get_samples(x)
  if (has_groups(x)) z$groups <- get_groups(x)
  z
}

#' @method as.data.frame LogRatio
#' @export
as.data.frame.LogRatio <- function(x, ...) {
  z <- as.data.frame(methods::as(x, "matrix"))

  z
}

#' @method as.data.frame OutlierIndex
#' @export
as.data.frame.OutlierIndex <- function(x, ...) {
  z <- data.frame(
    index = seq_along(x@outliers),
    samples = get_samples(x),
    distances = x@distances,
    outlier = get_outliers(x),
    stringsAsFactors = FALSE
  )
  if (has_groups(x)) z$groups <- get_groups(x)
  z
}
