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

    cols <- make_names(colnames(from), n = ncol(from), prefix = "col")
    auto <- getOption("nexus.autodetect")
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
    quali <- arkhe::detect(data, is.numeric, 2, negate = TRUE)
    if (any(quali)) {
      old <- colnames(data)
      data <- data[, !quali, drop = FALSE]
      arkhe::assert_filled(data)

      ## Generate message
      tot <- sum(quali)
      msg <- "%d qualitative %s removed: %s."
      txt <- ngettext(tot, "variable was", "variables were")
      col <- paste(old[quali], collapse = ", ")
      message(sprintf(msg, tot, txt, col))
    }

    ## Build matrix
    data <- data.matrix(data, rownames.force = NA)
    totals <- rowSums(data, na.rm = TRUE)
    data <- data / totals
    # dimnames(data) <- make_dimnames(from)

    .CompositionMatrix(data, totals = totals, samples = spl, groups = grp)
  }
)

make_names <- function(x, n = 0, prefix = "var") {
  if (is.null(x)) {
    x <- if (n > 0) paste0(prefix, seq_len(n)) else character(0)
  } else {
    x <- make.unique(as.character(x), sep = "_")
  }
  x
}

make_dimnames <- function(x) {
  list(
    make_names(dimnames(x)[[1L]], nrow(x), "row"),
    make_names(dimnames(x)[[2L]], ncol(x), "col")
  )
}

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
  y <- methods::S3Part(x, strictS3 = TRUE)
  data.frame(
    index = seq_along(y),
    sample = get_samples(x),
    group = if (has_groups(x)) get_groups(x) else NA_character_,
    distance = x@distances,
    outlier = y,
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}
