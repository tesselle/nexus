# OUTLIERS
#' @include AllGenerics.R
NULL

# Find =========================================================================
#' @export
#' @rdname outliers
#' @aliases outliers,CompositionMatrix-method
setMethod(
  f = "outliers",
  signature = c(object = "CompositionMatrix"),
  definition = function(object, ..., groups = NULL,
                        method = c("mve", "mcd"), quantile = 0.975) {
    ## Transformation
    z <- transform_ilr(object)

    ## Grouping
    m <- nrow(z)
    p <- ncol(z)
    groups <- get_variable(object, which = groups)
    if (is.null(groups) || all(is.na(groups))) {
      grp <- list(z)
      groups <- list(seq_len(m))
    } else {
      arkhe::assert_length(groups, m)
      grp <- split(z, f = groups)
      groups <- split(seq_len(m), f = groups)
    }

    ## Clean
    size <- vapply(X = grp, FUN = nrow, FUN.VALUE = integer(1), USE.NAMES = TRUE)
    too_small <- size < (p + 1)
    very_small <- size < (2 * p)
    if (all(too_small)) {
      stop("Too few samples.", call. = FALSE)
    }
    if (any(too_small)) {
      msg <- "%s is ignored: sample size is too small (%d).\n"
      warning(sprintf(msg, names(grp)[too_small], size[too_small]), call. = FALSE)
    }
    if (any(very_small)) {
      msg <- "%s: possibly too small sample size (%d).\n"
      warning(sprintf(msg, names(grp)[very_small], size[very_small]), call. = FALSE)
    }

    dc <- dr <- matrix(data = NA_real_, nrow = m, ncol = length(grp),
                       dimnames = list(rownames(object), names(grp)))

    ## For each group...
    for (i in seq_along(grp)[!too_small]) {
      ## ...subset
      tmp <- grp[[i]]

      ## ...compute center and spread
      ## Standard estimators
      estc <- list(center = colMeans(tmp, na.rm = TRUE), cov = cov(tmp))

      ## Robust estimators
      method <- match.arg(method, several.ok = FALSE)
      estr <- MASS::cov.rob(tmp, method = method, ...)

      ## ...Mahalanobis distance
      dc[, i] <- stats::mahalanobis(z, center = estc$center, cov = estc$cov)
      dr[, i] <- stats::mahalanobis(z, center = estr$center, cov = estr$cov)
    }

    ## Threshold
    limit <- sqrt(stats::qchisq(p = quantile, df = p))

    .OutlierIndex(
      groups = groups,

      standard = sqrt(dc),
      robust = sqrt(dr),
      limit = limit,
      dof = p
    )
  }
)
