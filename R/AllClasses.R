# CLASSES DEFINITION AND INITIALIZATION
NULL

# Transformations ==============================================================
#' Log-Ratio
#'
#' S4 classes to represent log-ratio data transformations.
#' @slot parts A [`character`] vector XXX.
#' @slot base A [`numeric`] vector XXX.
#' @slot mean A [`numeric`] vector XXX.
#' @slot rationing_values A [`numeric`] vector XXX.
#' @slot rationing_index An [`integer`] vector XXX.
#' @slot pivot An [`integer`] vector XXX.
#' @slot ratio A [`character`] vector XXX.
#' @slot norm A [`numeric`] vector XXX.
#' @note
#'  These classes inherit from [`matrix`].
#' @author N. Frerebeau
#' @family matrix
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
    base = "numeric"
  ),
  contains = "NumericMatrix"
)

#' @rdname LogRatio
#' @aliases CLR-class
.CLR <- setClass(
  Class = "CLR",
  slots = c(
    mean = "numeric"
  ),
  contains = "LogRatio"
)

#' @rdname LogRatio
#' @aliases ALR-class
.ALR <- setClass(
  Class = "ALR",
  slots = c(
    rationing_values = "numeric",
    rationing_index = "integer"
  ),
  contains = "LogRatio"
)

#' @rdname LogRatio
#' @aliases ILR-class
.ILR <- setClass(
  Class = "ILR",
  slots = c(
    pivot = "integer",
    ratio = "character",
    norm = "numeric"
  ),
  contains = "LogRatio"
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
