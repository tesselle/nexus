# CLASSES DEFINITION AND INITIALIZATION
NULL

# CompositionMatrix ============================================================
#' Numeric Matrix
#'
#' S4 classes that represent a \eqn{m \times p}{m x p} numeric matrix.
#' @note
#'  This class inherits from [`matrix`].
#' @author N. Frerebeau
#' @family classes
#' @docType class
#' @rdname NumericMatrix
#' @aliases NumericMatrix-class
#' @keywords internal
.NumericMatrix <- setClass(
  Class = "NumericMatrix",
  contains = "matrix"
)

#' Compositional Matrix
#'
#' An S4 class to represent compositional data.
#' @slot totals A [`numeric`] vector to store the absolute row sums (before
#'  the closure of the compositions).
#' @slot samples A [`character`] vector to store the sample identifiers
#'  (allows duplicates in case of replicated measurements).
#' @slot groups A [`character`] vector to store the group names (if any).
#' @note
#'  This class inherits from [`matrix`].
#' @seealso [as_composition()]
#' @example inst/examples/ex-matrix.R
#' @author N. Frerebeau
#' @family classes
#' @docType class
#' @aliases CompositionMatrix-class
.CompositionMatrix <- setClass(
  Class = "CompositionMatrix",
  slots = c(
    totals = "numeric",
    samples = "character",
    groups = "character"
  ),
  contains = c("NumericMatrix")
)

# Transformations ==============================================================
#' Log-Ratio Matrix
#'
#' S4 classes to represent log-ratio data transformations.
#' @slot parts A [`character`] vector to store the part names.
#' @slot ratio A [`character`] vector to store the ratio names.
#' @slot order An [`integer`] vector to store the original ordering of the
#'  columns.
#' @slot base A [`numeric`] matrix to store the basis of the transformation.
#' @slot weights A [`numeric`] vector to store the weights assigned to the
#'  respective log-ratios.
#' @note
#'  These classes inherit from [`matrix`].
#' @seealso [transform_lr()], [transform_clr()], [transform_alr()],
#'  [transform_ilr()], [transform_plr()]
#' @author N. Frerebeau
#' @family classes
#' @docType class
#' @aliases LogRatio-class
.LogRatio <- setClass(
  Class = "LogRatio",
  slots = c(
    totals = "numeric",
    samples = "character",
    groups = "character",

    parts = "character",
    ratio = "character",
    order = "integer",
    base = "matrix",
    weights = "numeric"
  ),
  contains = "NumericMatrix"
)

#' @rdname LogRatio-class
#' @aliases LR-class
.LR <- setClass(
  Class = "LR",
  contains = "LogRatio"
)

#' @rdname LogRatio-class
#' @aliases CLR-class
.CLR <- setClass(
  Class = "CLR",
  contains = "LogRatio"
)

#' @rdname LogRatio-class
#' @aliases ALR-class
.ALR <- setClass(
  Class = "ALR",
  contains = "LogRatio"
)

#' @rdname LogRatio-class
#' @aliases ILR-class
.ILR <- setClass(
  Class = "ILR",
  contains = "LogRatio"
)

#' @rdname LogRatio-class
#' @aliases PLR-class
.PLR <- setClass(
  Class = "PLR",
  contains = "ILR"
)

# OutliersIndex ================================================================
#' Outliers
#'
#' An S4 class to store the result of outlier detection.
#' @slot samples A [`character`] vector XXX.
#' @slot groups A [`character`] vector XXX.
#' @slot distances A [`numeric`] vector XXX.
#' @slot outliers A [`logical`] vector giving the squared Mahalanobis distance.
#' @slot limit An [`numeric`] vector XXX.
#' @slot robust An [`logical`] vector XXX.
#' @author N. Frerebeau
#' @family classes
#' @docType class
#' @aliases OutlierIndex-class
.OutlierIndex <- setClass(
  Class = "OutlierIndex",
  slots = c(
    samples = "character",
    groups = "character",
    distances = "numeric",
    outliers = "logical",
    limit = "numeric",
    robust = "logical",
    df = "integer"
  )
)
