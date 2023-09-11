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
    totals <- rowSums(from, na.rm = TRUE)
    from <- from / totals
    dimnames(from) <- make_dimnames(from)

    samples <- rownames(from)
    groups <- rep(NA_character_, nrow(from))
    .CompositionMatrix(from, totals = totals, samples = samples, groups = groups)
  }
)

#' @export
#' @rdname as_composition
#' @aliases as_composition,data.frame-method
setMethod(
  f = "as_composition",
  signature = c(from = "data.frame"),
  definition = function(from, samples = NULL, groups = NULL,
                        verbose = getOption("nexus.verbose")) {

    cols <- make_names(colnames(from), n = ncol(from), prefix = "V")
    empty <- rep(NA_character_, nrow(from))
    auto <- getOption("nexus.autodetect")
    index <- function(what, where) {
      grep(what, where, ignore.case = TRUE, value = FALSE)
    }

    ## Samples
    spl <- rownames(from) %||% empty
    if (is.null(samples) && auto) samples <- index("^sample[s]{0,1}$", cols)
    if (length(samples) == 1) spl <- as.character(from[[samples]])
    n_spl <- length(unique(spl[!is.na(spl)]))

    ## Groups
    grp <- empty
    if (is.null(groups) && auto) groups <- index("^group[s]{0,1}$", cols)
    if (length(groups) == 1) grp <- as.character(from[[groups]])
    n_grp <- length(unique(grp[!is.na(grp)]))

    ## Drop extra columns (if any)
    drop <- c(samples, groups)
    data <- if (length(drop) > 0) from[, -drop, drop = FALSE] else from
    if (verbose && n_spl > 0) {
      msg <- ngettext(n_spl, "sample was", "samples were")
      message(sprintf("%d unique %s found.", n_spl, msg))
    }
    if (verbose && n_grp > 0) {
      msg <- ngettext(n_grp, "group was", "groups were")
      message(sprintf("%d %s found.", n_grp, msg))
    }
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

make_names <- function(x, n, prefix = "X") {
  x <- if (n > 0) x %||% paste0(prefix, seq_len(n)) else character(0)
  x <- make.unique(x, sep = "_")
  x
}

make_dimnames <- function(x) {
  list(
    make_names(dimnames(x)[[1L]], nrow(x), "S"),
    make_names(dimnames(x)[[2L]], ncol(x), "V")
  )
}

#' @export
#' @rdname as_amounts
#' @aliases as_amounts,CompositionMatrix-method
setMethod(
  f = "as_amounts",
  signature = c(from = "CompositionMatrix"),
  definition = function(from) {
    from@.Data * from@totals
  }
)

# To data.frame ================================================================
#' @method as.data.frame CompositionMatrix
#' @export
as.data.frame.CompositionMatrix <- function(x, ...) {
  z <- as.data.frame(methods::as(x, "matrix"))

  z$samples <- get_samples(x)
  z$groups <- get_groups(x)
  z
}

#' @method as.data.frame LogRatio
#' @export
as.data.frame.LogRatio <- function(x, ...) {
  z <- as.data.frame(methods::as(x, "matrix"))

  z$samples <- get_samples(x)
  z$groups <- get_groups(x)
  z
}

#' @method as.data.frame OutlierIndex
#' @export
as.data.frame.OutlierIndex <- function(x, ...) {
  y <- methods::S3Part(x, strictS3 = TRUE)
  data.frame(
    index = seq_along(y),
    sample = get_samples(x),
    group = get_groups(x),
    distance = x@distances,
    outlier = y,
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}
