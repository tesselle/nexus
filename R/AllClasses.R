# CLASSES DEFINITION AND INITIALIZATION
NULL

# Register S3 classes ==========================================================
setOldClass("dist")

## Index vectors
## (for 'i' in x[i], x[i, ], x[, i], etc.)
setClassUnion("index", members = c("logical", "numeric", "character"))

# ReferenceGroups ==============================================================
#' Grouped Data
#'
#' A virtual S4 class to represent reference groups.
#' @slot group_indices An [`integer`] vector to store the group that each value
#'  belongs to.
#' @slot group_levels A [`character`] vector to store the values of the grouping
#'  variables.
#' @example inst/examples/ex-matrix.R
#' @author N. Frerebeau
#' @family classes
#' @docType class
#' @aliases ReferenceGroups-class
#' @keywords internal
.ReferenceGroups <- setClass(
  Class = "ReferenceGroups",
  slots = c(
    group_indices = "integer",
    group_levels = "character"
  ),
  contains = c("VIRTUAL")
)

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

#' Compositional Matrix
#'
#' An S4 class to represent compositional data.
#' @slot totals A [`numeric`] vector to store the absolute row sums (before
#'  the closure of the compositions).
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
#'  This class inherits from [`NumericMatrix-class`].
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
    totals = "numeric"
  ),
  contains = c("NumericMatrix")
)

#' Grouped Compositional Matrix
#'
#' An S4 class to represent grouped compositional data.
#' @section Coerce:
#'  In the code snippets below, `x` is a `GroupedComposition` object.
#'  \describe{
#'   \item{`as.data.frame(x)`}{Coerces to a [`data.frame`].}
#'  }
#' @note
#'  This class inherits from [`CompositionMatrix-class`] and
#'  [`ReferenceGroups-class`].
#' @seealso [as_composition()]
#' @example inst/examples/ex-matrix.R
#' @author N. Frerebeau
#' @family classes
#' @docType class
#' @aliases GroupedComposition-class
#' @keywords internal
.GroupedComposition <- setClass(
  Class = "GroupedComposition",
  contains = c("CompositionMatrix", "ReferenceGroups")
)

# Transformations ==============================================================
#' Log-Ratio Matrix
#'
#' S4 classes to represent log-ratio data transformations.
#' @slot totals A [`numeric`] vector to store the absolute row sums (before
#'  the closure of the compositions).
#' @slot parts A [`character`] vector to store the original part names.
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

#' Grouped Log-Ratio Matrix
#'
#' An S4 class to represent grouped log-ratio.
#' @section Coerce:
#'  In the code snippets below, `x` is a `GroupedLogRatio` object.
#'  \describe{
#'   \item{`as.data.frame(x)`}{Coerces to a [`data.frame`].}
#'  }
#' @note
#'  This class inherits from [`LogRatio-class`] and
#'  [`ReferenceGroups-class`].
#' @example inst/examples/ex-matrix.R
#' @author N. Frerebeau
#' @family classes
#' @docType class
#' @name GroupedLogRatio-class
#' @rdname GroupedLogRatio-class
#' @keywords internal
NULL

#' @rdname GroupedLogRatio-class
#' @aliases GroupedLR-class
.GroupedLR <- setClass(
  Class = "GroupedLR",
  contains = c("LR", "ReferenceGroups")
)

#' @rdname GroupedLogRatio-class
#' @aliases GroupedCLR-class
.GroupedCLR <- setClass(
  Class = "GroupedCLR",
  contains = c("CLR", "ReferenceGroups")
)

#' @rdname GroupedLogRatio-class
#' @aliases GroupedALR-class
.GroupedALR <- setClass(
  Class = "GroupedALR",
  contains = c("ALR", "ReferenceGroups")
)

#' @rdname GroupedLogRatio-class
#' @aliases GroupedILR-class
.GroupedILR <- setClass(
  Class = "GroupedILR",
  contains = c("ILR", "ReferenceGroups")
)

#' @rdname GroupedLogRatio-class
#' @aliases GroupedPLR-class
.GroupedPLR <- setClass(
  Class = "GroupedPLR",
  contains = c("PLR", "ReferenceGroups")
)

setClassUnion(
  name = "GroupedLogRatio",
  members = c("GroupedLR", "GroupedCLR", "GroupedALR", "GroupedILR", "GroupedPLR")
)

# OutlierIndex =================================================================
#' Outliers
#'
#' An S4 class to store the result of outlier detection.
#' @slot samples A [`character`] vector to store the sample identifiers.
#' @slot standard A [`numeric`] matrix giving the standard squared Mahalanobis
#'  distances.
#' @slot robust A [`numeric`] matrix giving the robust squared Mahalanobis
#'  distances.
#' @slot limit A [`numeric`] value giving the cut-off value used for outliers
#'  detection (quantile of the Chi-squared distribution).
#' @slot dof A (non-negative) [`numeric`] value giving the degrees of freedom.
#' @section Coerce:
#'  In the code snippets below, `x` is an `OutlierIndex` object.
#'  \describe{
#'   \item{`as.data.frame(x)`}{Coerces to a [`data.frame`].}
#'  }
#' @author N. Frerebeau
#' @family classes
#' @docType class
#' @aliases OutlierIndex-class
#' @keywords internal
.OutlierIndex <- setClass(
  Class = "OutlierIndex",
  slots = c(
    samples = "character",
    standard = "numeric",
    robust = "numeric",
    limit = "numeric",
    dof = "integer"
  )
)
