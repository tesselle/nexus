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
  definition = function(object, ..., groups = get_groups(object), robust = TRUE,
                        method = c("mve", "mcd"), quantile = 0.975) {
    ## Transformation
    z <- transform_ilr(object)

    ## Grouping
    m <- nrow(z)
    p <- ncol(z)
    if (is.null(groups) || all(is.na(groups))) {
      grp <- list(z)
      groups <- rep(NA_character_, m)
    } else {
      grp <- split(z, f = groups)
    }

    ## Clean
    size <- vapply(X = grp, FUN = nrow, FUN.VALUE = integer(1), USE.NAMES = TRUE)
    too_small <- size < p
    if (any(too_small)) {
      msg <- "%s is ignored: sample size is too small (%d).\n"
      warning(sprintf(msg, names(grp)[too_small], size[too_small]), call. = FALSE)
    }
    if (all(too_small)) {
      stop("Too few samples.", call. = FALSE)
    }
    grp <- grp[!too_small]

    d2 <- matrix(data = NA_real_, nrow = m, ncol = length(grp))
    dimnames(d2) <- list(rownames(object), names(grp))

    ## For each group...
    for (i in seq_along(grp)) {
      ## ...subset
      tmp <- grp[[i]]

      ## ...compute center and spread
      if (!robust) method <- "classical" # Standard estimators
      else method <- match.arg(method, several.ok = FALSE) # Robust estimators
      est <- MASS::cov.rob(tmp, method = method, ...)

      ## Mahalanobis distance
      d2[, i] <- stats::mahalanobis(z, center = est$center, cov = est$cov)
    }

    ## Threshold
    dof <- p - 1L
    limit <- sqrt(stats::qchisq(p = quantile, df = dof))

    d <- sqrt(d2)

    .OutlierIndex(
      d,
      samples = get_samples(object),
      groups = as.character(groups),
      limit = limit,
      robust = robust,
      dof = dof
    )
  }
)
