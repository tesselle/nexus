# CLASSES DEFINITION AND INITIALIZATION
NULL

# Transformations ==============================================================
#' Log-Ratio
#'
#' S4 classes to represent log-ratio data transformations.
#' @slot parts A [`character`] vector XXX.
#' @slot base A [`numeric`] vector XXX.
#' @slot mean A [`numeric`] vector XXX.
#' @slot denominator An [`integer`] vector XXX.
#' @slot pivot An [`integer`] vector XXX.
#' @slot ratio A [`character`] vector XXX.
#' @slot norm A [`numeric`] vector XXX.
#' @note
#'  These classes inherit from [`matrix`].
#' @author N. Frerebeau
#' @family log-ratio transformations
#' @docType class
#' @name LogRatio
#' @rdname LogRatio
NULL

#' @rdname LogRatio
#' @aliases LogRatio-class
.LogRatio <- setClass(
  Class = "LogRatio",
  slots = c(
    parts = "character",
    weights = "numeric"
  ),
  contains = "NumericMatrix"
)

#' @rdname LogRatio
#' @aliases CLR-class
.CLR <- setClass(
  Class = "CLR",
  contains = "LogRatio"
)

#' @rdname LogRatio
#' @aliases ALR-class
.ALR <- setClass(
  Class = "ALR",
  slots = c(
    denominator = "integer"
  ),
  contains = "LogRatio"
)

#' @rdname LogRatio
#' @aliases ILR-class
.ILR <- setClass(
  Class = "ILR",
  slots = c(
    ratio = "character",
    base = "matrix"
  ),
  contains = "LogRatio"
)

#' @rdname LogRatio
#' @aliases PLR-class
.PLR <- setClass(
  Class = "PLR",
  slots = c(
    order = "integer"
  ),
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
#' @family outlier detection methods
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
    robust = "logical"
  )
)
