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

    spl <- make_names(x = NULL, n = nrow(from), prefix = "S")
    lab <- if (has_rownames(from)) rownames(from) else make_codes(spl)
    grp <- rep(NA_character_, nrow(from))

    rownames(from) <- lab
    colnames(from) <- make_names(x = colnames(from), n = ncol(from), prefix = "V")

    .CompositionMatrix(from, totals = totals, codes = lab,
                       samples = spl, groups = grp)
  }
)

#' @export
#' @rdname as_composition
#' @aliases as_composition,data.frame-method
setMethod(
  f = "as_composition",
  signature = c(from = "data.frame"),
  definition = function(from, codes = NULL, samples = NULL, groups = NULL,
                        auto = getOption("nexus.autodetect"),
                        verbose = getOption("nexus.verbose")) {

    cols <- colnames(from)
    empty <- rep(NA_character_, nrow(from))

    index <- function(what, where) {
      grep(what, where, ignore.case = TRUE, value = FALSE)
    }

    ## Sample names
    spl <- make_names(x = NULL, n = nrow(from), prefix = "S")
    if (is.null(samples) && auto) samples <- index("^sample[s]{0,1}$", cols)
    if (length(samples) == 1) {
      if (is.character(samples)) samples <- match(samples, cols)
      spl <- as.character(from[[samples]])
    }

    ## Identifiers (must be unique)
    lab <- if (has_rownames(from)) rownames(from) else make_codes(spl)
    if (is.null(codes) && auto) codes <- index("^(code[s]{0,1}|identifier[s]{0,1})$", cols)
    if (length(codes) == 1) {
      if (is.character(codes)) codes <- match(codes, cols)
      else lab <- as.character(from[[codes]])
    }

    ## Group names
    grp <- empty
    if (is.null(groups) && auto) groups <- index("^group[s]{0,1}$", cols)
    if (length(groups) == 1) {
      if (is.character(groups)) groups <- match(groups, cols)
      grp <- as.character(from[[groups]])
      grp[grp == ""] <- NA_character_
    }

    ## Drop extra columns (if any)
    drop <- c(codes, samples, groups)
    data <- if (length(drop) > 0) from[, -drop, drop = FALSE] else from

    ## Print messages
    if (verbose) {
      dupli <- duplicated(spl, fromLast = FALSE) | duplicated(spl, fromLast = TRUE)
      n_spl <- sum(dupli)
      if (n_spl > 0) {
        msg <- ngettext(n_spl, "measurement was", "measurements were")
        message(sprintf("%d replicated %s found.", n_spl, msg))
      }
      n_grp <- length(unique(grp[!is.na(grp)]))
      if (n_grp > 0) {
        msg <- ngettext(n_grp, "group was", "groups were")
        message(sprintf("%d %s found.", n_grp, msg))
      }
    }
    arkhe::assert_filled(data)

    ## Remove non-numeric columns
    data <- arkhe::keep_cols(x = data, f = is.numeric, all = FALSE,
                             verbose = verbose)

    ## Build matrix
    data <- data.matrix(data, rownames.force = NA)
    totals <- rowSums(data, na.rm = TRUE)
    data <- data / totals
    rownames(data) <- lab

    .CompositionMatrix(data, totals = totals, codes = lab,
                       samples = spl, groups = grp)
  }
)

make_codes <- function(x) {
  if (!any(duplicated(x))) return(x)
  x <- split(x = seq_along(x), f = x)
  nm <- rep(names(x), lengths(x))
  nm <- tapply(
    X = nm,
    INDEX = nm,
    FUN = function(x) paste(x, seq_along(x), sep = "_"),
    simplify = FALSE
  )

  x <- unlist(x, use.names = FALSE)
  nm <- unlist(nm, use.names = FALSE)
  nm[x]
}

make_names <- function(x, n = length(x), prefix = "X") {
  x <- if (n > 0) x %||% paste0(prefix, seq_len(n)) else character(0)
  x <- make.unique(x, sep = "_")
  x
}

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
    data.frame(
      identifier = get_identifiers(from),
      sample = get_samples(from),
      group = get_groups(from),
      from,
      row.names = NULL
    )
  }
)

#' @export
#' @rdname as_features
#' @aliases as_features,LogRatio-method
setMethod(
  f = "as_features",
  signature = c(from = "LogRatio"),
  definition = function(from) {
    data.frame(
      identifier = get_identifiers(from),
      sample = get_samples(from),
      group = get_groups(from),
      from,
      row.names = NULL
    )
  }
)

# To data.frame ================================================================
#' @method as.data.frame CompositionMatrix
#' @export
as.data.frame.CompositionMatrix <- function(x, ...) {
  as.data.frame(methods::as(x, "matrix"), row.names = get_identifiers(x))
}

#' @method as.data.frame LogRatio
#' @export
as.data.frame.LogRatio <- function(x, ...) {
  as.data.frame(methods::as(x, "matrix"), row.names = get_identifiers(x))
}

#' @method as.data.frame OutlierIndex
#' @export
as.data.frame.OutlierIndex <- function(x, ...) {
  out <- methods::as(x, "matrix")
  colnames(out) <- paste0("out_", colnames(out))
  d <- x@distances
  colnames(d) <- paste0("dist_", colnames(d))
  data.frame(
    index = seq_len(nrow(out)),
    sample = get_samples(x),
    group = get_groups(x),
    d,
    out,
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}
