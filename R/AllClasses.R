# CLASSES DEFINITION AND INITIALIZATION
NULL

# Register S3 classes ==========================================================
setOldClass("dist")

## Index vectors
## (for 'i' in x[i], x[i, ], x[, i], etc.)
setClassUnion("index", members = c("logical", "numeric", "character"))

# CompositionMatrix ============================================================
#' Numeric Matrix
#'
#' S4 classes that represent a \eqn{m \times p}{m x p} numeric matrix.
#' @slot .Data A \eqn{m \times p}{m x p} `numeric` [`matrix`].
#' @note
#'  This class inherits from [`matrix`].
#' @author N. Frerebeau
#' @family classes
#' @docType class
#' @aliases NumericMatrix-class
#' @keywords internal
.NumericMatrix <- setClass(
  Class = "NumericMatrix",
  contains = "matrix"
)

#' Logical Matrix
#'
#' S4 classes that represent a \eqn{m \times p}{m x p} logical matrix.
#' @slot .Data A \eqn{m \times p}{m x p} `logical` [`matrix`].
#' @note
#'  This class inherits from [`matrix`].
#' @author N. Frerebeau
#' @family classes
#' @docType class
#' @aliases LogicalMatrix-class
#' @keywords internal
.LogicalMatrix <- setClass(
  Class = "LogicalMatrix",
  contains = "matrix"
)

#' Compositional Matrix
#'
#' An S4 class to represent compositional data.
#' @slot totals A [`numeric`] vector to store the absolute row sums (before
#'  the closure of the compositions).
#' @slot samples A [`character`] vector to store the sample identifiers
#'  (allows duplicates in case of repeated measurements).
#' @slot groups A [`character`] vector to store the group names.
#' @section Coerce:
#'  In the code snippets below, `x` is a `CompositionMatrix` object.
#'  \describe{
#'   \item{`as.data.frame(x)`}{Coerces to a [`data.frame`].}
#'  }
#' @section Subset:
#'  In the code snippets below, `x` is a `CompositionMatrix` object.
#'  \describe{
#'   \item{`x[i, j]`}{Extract parts of a matrix (see [`[`][subset]).}
#'  }
#' @note
#'  This class inherits from [`matrix`].
#' @seealso [as_composition()]
#' @example inst/examples/ex-matrix.R
#' @author N. Frerebeau
#' @family classes
#' @docType class
#' @aliases CompositionMatrix-class
#' @keywords internal
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
#' @slot totals A [`numeric`] vector to store the absolute row sums (before
#'  the closure of the compositions).
#' @slot samples A [`character`] vector to store the sample identifiers
#'  (allows duplicates in case of repeated measurements).
#' @slot groups A [`character`] vector to store the group names.
#' @slot parts A [`character`] vector to store the part names.
#' @slot ratio A [`character`] vector to store the ratio names.
#' @slot order An [`integer`] vector to store the original ordering of the
#'  columns.
#' @slot base A [`numeric`] matrix to store the basis of the transformation.
#' @slot weights A [`numeric`] vector to store the weights assigned to the
#'  respective log-ratios.
#' @section Coerce:
#'  In the code snippets below, `x` is a `LogRatio` object.
#'  \describe{
#'   \item{`as.data.frame(x)`}{Coerces to a [`data.frame`].}
#'  }
#' @note
#'  These classes inherit from [`matrix`].
#' @seealso [transform_lr()], [transform_clr()], [transform_alr()],
#'  [transform_ilr()], [transform_plr()]
#' @author N. Frerebeau
#' @family classes
#' @docType class
#' @aliases LogRatio-class
#' @keywords internal
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

# OutlierIndex =================================================================
#' Outliers
#'
#' An S4 class to store the result of outlier detection.
#' @slot .Data A [`logical`] matrix.
#' @slot samples A [`character`] vector to store the sample identifiers
#'  (allows duplicates in case of repeated measurements).
#' @slot groups A [`character`] vector to store the group names.
#' @slot distances A [`numeric`] matrix giving the squared Mahalanobis distance.
#' @slot limit A [`numeric`] value giving the cut-off value used for outlier
#'  detection (quantile of the Chi-squared distribution).
#' @slot robust An [`logical`] scalar: were robust estimators used?
#' @slot dof A (non-negative) [`numeric`] value giving the degrees of freedom.
#' @section Coerce:
#'  In the code snippets below, `x` is an `OutlierIndex` object.
#'  \describe{
#'   \item{`as.data.frame(x)`}{Coerces to a [`data.frame`].}
#'  }
#' @note
#'  This class inherits from [`logical`].
#' @author N. Frerebeau
#' @family classes
#' @docType class
#' @aliases OutlierIndex-class
#' @keywords internal
.OutlierIndex <- setClass(
  Class = "OutlierIndex",
  slots = c(
    samples = "character",
    groups = "character",
    distances = "matrix",
    limit = "numeric",
    robust = "logical",
    dof = "integer"
  ),
  contains = "matrix"
)
