# CLASSES DEFINITION AND INITIALIZATION
NULL

# Transformations ==============================================================
#' Log-Ratio
#'
#' S4 classes to represent log-ratio data transformations.
#' @slot parts A \code{\link{character}} vector XXX.
#' @slot base A \code{\link{numeric}} vector XXX.
#' @slot mean A \code{\link{numeric}} vector XXX.
#' @slot rationing_values A \code{\link{numeric}} vector XXX.
#' @slot rationing_index An \code{\link{integer}} vector XXX.
#' @slot pivot An \code{\link{integer}} vector XXX.
#' @slot ratio A \code{\link{character}} vector XXX.
#' @slot norm A \code{\link{numeric}} vector XXX.
#' @note
#'  These classes inherit from \code{\link{matrix}}.
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
