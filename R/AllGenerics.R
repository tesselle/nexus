# GENERIC METHODS
#' @include AllClasses.R
NULL

# Extract ======================================================================
## Mutators --------------------------------------------------------------------
#' Get or Set Parts of an Object
#'
#' Getters and setters to retrieve or set parts of an object.
#' @param x An object from which to get or set element(s) (typically a
#'  `*Matrix` object).
#' @param value A possible value for the element(s) of `x`.
#' @return
#'  An object of the same sort as `x` with the new values assigned.
#' @author N. Frerebeau
#' @docType methods
#' @family mutator
#' @name mutator
#' @rdname mutator
#' @aliases get set
NULL

#' @rdname mutator
#' @aliases get_outliers-method
setGeneric(
  name = "get_outliers",
  def = function(x) standardGeneric("get_outliers")
)

## Subset ----------------------------------------------------------------------
#' Extract or Replace Parts of an Object
#'
#' Operators acting on objects to extract or replace parts.
#' @param x An object from which to extract element(s) or in which to replace
#'  element(s).
#' @param i A \code{\link{character}} string specifying elements to extract.
#'  Any unambiguous substring can be given (see details).
#' @return
#'  A subsetted object.
#' @author N. Frerebeau
#' @docType methods
#' @family mutator
#' @name subset
#' @rdname subset
NULL

# Outliers =====================================================================
#' Outlier Detection
#'
#' @param object A \linkS4class{CompositionMatrix} object.
#' @param level A length-one \code{\link{numeric}} vector giving the
#'  significance level. \code{level} is used as a cut-off value for outlier
#'  detection: observations with larger (squared) Mahalanobis distance are
#'  considered as potential outliers.
#' @param robust A \code{\link{logical}} scalar: should robust estimators be
#'  used (see \code{\link[robustbase]{covMcd}})?
#' @param ... Currently not used.
#' @return
#'  \code{find_outliers} returns an \linkS4class{OutlierIndex} object.
#' @references
#'  Filzmoser, P., Garrett, R. G. & Reimann, C. (2005). Multivariate outlier
#'  detection in exploration geochemistry. \emph{Computers & Geosciences},
#'  31(5), 579-587. \doi{10.1016/j.cageo.2004.11.013}.
#'
#'  Filzmoser, P. & Hron, K. (2008). Outlier Detection for Compositional Data
#'  Using Robust Methods. \emph{Mathematical Geosciences}, 40(3), 233-248.
#'  \doi{10.1007/s11004-007-9141-5}.
#'
#'  Filzmoser, P., Hron, K. & Reimann, C. (2012). Interpretation of multivariate
#'  outliers for compositional data. \emph{Computers & Geosciences}, 39, 77-85.
#'  \doi{10.1016/j.cageo.2011.06.014}.
#' @example inst/examples/ex-outliers.R
#' @author N. Frerebeau
#' @docType methods
#' @family outliers
#' @name outliers
#' @rdname outliers
NULL

#' @rdname outliers
#' @aliases find_outliers-method
setGeneric(
  name = "find_outliers",
  def = function(object, ...) standardGeneric("find_outliers")
)

#' @rdname outliers
#' @aliases count_outliers-method
setGeneric(
  name = "count_outliers",
  def = function(object) standardGeneric("count_outliers")
)

#' @rdname outliers
#' @aliases plot_outliers-method
setGeneric(
  name = "plot_outliers",
  def = function(object, data, ...) standardGeneric("plot_outliers")
)

# Transform ====================================================================
#' Data Transformation
#'
#' @param object A \linkS4class{ChemicalMatrix} object.
#' @param j An \code{\link{integer}} giving the index of the rationing part.
#' @param pivot An \code{\link{integer}} giving the index of the pivotal
#'  variable.
#' @param base A positive \code{\link{numeric}} value giving the base with
#'  respect to which logarithms are computed. Change this only if you know
#'  what your are doing.
#' @param ... Currently not used.
#' @return
#'  A \linkS4class{LogRatio} object.
#' @references
#'  Aitchison, J. (1986). \emph{The Statistical Analysis of Compositional Data}.
#'  London: Chapman and Hall.
#'  DOI: \href{https://dx.doi.org/10.1007/978-94-009-4109-0}{10.1007/978-94-009-4109-0}.
#'
#'  Egozcue, J. J., Pawlowsky-Glahn, V., Mateu-Figueras, G. & Barceló-Vidal, C.
#'  (2003). Isometric Logratio Transformations for Compositional Data Analysis.
#'  \emph{Mathematical Geology}, 35(3), 279-300.
#'  DOI: \href{https://dx.doi.org/10.1023/A:1023818214614}{10.1023/A:1023818214614}.
#'
#'  Fišerová, E. & Hron, K. (2011). On the Interpretation of Orthonormal
#'  Coordinates for Compositional Data. \emph{Mathematical Geosciences}, 43(4),
#'  455‑468. DOI:
#'  \href{https://doi.org/10.1007/s11004-011-9333-x}{10.1007/s11004-011-9333-x}.
#' @example inst/examples/ex-transform.R
#' @author N. Frerebeau
#' @docType methods
#' @family transformation
#' @name transform
#' @rdname transform
NULL

#' @rdname transform
#' @aliases transform_clr-method
setGeneric(
  name = "transform_clr",
  def = function(object, ...) standardGeneric("transform_clr"),
  valueClass = "LogRatio"
)

#' @rdname transform
#' @aliases transform_alr-method
setGeneric(
  name = "transform_alr",
  def = function(object, ...) standardGeneric("transform_alr"),
  valueClass = "LogRatio"
)

#' @rdname transform
#' @aliases transform_ilr-method
setGeneric(
  name = "transform_ilr",
  def = function(object, ...) standardGeneric("transform_ilr"),
  valueClass = "LogRatio"
)

#' @rdname transform
#' @aliases transform_pivot-method
setGeneric(
  name = "transform_pivot",
  def = function(object, ...) standardGeneric("transform_pivot"),
  valueClass = "LogRatio"
)
