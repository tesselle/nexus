# GENERIC METHODS
#' @include AllClasses.R
NULL

# Set generics from other packages =============================================
setGeneric("has_groups", package = "arkhe")
setGeneric("get_groups", package = "arkhe")

# Extract ======================================================================
## Mutators --------------------------------------------------------------------
#' Get or Set Parts of an Object
#'
#' Getters and setters to retrieve or set parts of an object.
#' @param x An object from which to get or set element(s) (typically a `*Matrix`
#'  object).
# @param value A possible value for the element(s) of `x`.
#' @return
#'  An object of the same sort as `x` with the new values assigned.
#' @author N. Frerebeau
#' @docType methods
#' @family mutators
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
#' @param i A [`character`] string specifying elements to extract.
#'  Any unambiguous substring can be given (see details).
#' @return
#'  A subsetted object.
#' @author N. Frerebeau
#' @docType methods
#' @family mutators
#' @name subset
#' @rdname subset
NULL

# Outliers =====================================================================
#' Outlier Detection
#'
#' @param object An [OutlierIndex-class] object.
#' @param level A length-one [`numeric`] vector giving the
#'  significance level. `level` is used as a cut-off value for outlier
#'  detection: observations with larger (squared) Mahalanobis distance are
#'  considered as potential outliers.
#' @param robust A [`logical`] scalar: should robust estimators be
#'  used (see [robustbase::covMcd()])?
#' @param alpha A length-one [`numeric`] vector controlling the size of the
#'  subsets over which the determinant is minimized (see
#'  [robustbase::covMcd()]). Only used if `robust` is `TRUE`.
#' @param data A [CompositionMatrix-class] object.
#' @param select A [`numeric`] or [`character`] vector giving the selection of
#'  the parts that are drawn.
#' @param ... Currently not used.
#' @return
#'  `find_outliers()` returns an [OutlierIndex-class] object.
#' @references
#'  Filzmoser, P., Garrett, R. G. & Reimann, C. (2005). Multivariate outlier
#'  detection in exploration geochemistry. *Computers & Geosciences*,
#'  31(5), 579-587. \doi{10.1016/j.cageo.2004.11.013}.
#'
#'  Filzmoser, P. & Hron, K. (2008). Outlier Detection for Compositional Data
#'  Using Robust Methods. *Mathematical Geosciences*, 40(3), 233-248.
#'  \doi{10.1007/s11004-007-9141-5}.
#'
#'  Filzmoser, P., Hron, K. & Reimann, C. (2012). Interpretation of multivariate
#'  outliers for compositional data. *Computers & Geosciences*, 39, 77-85.
#'  \doi{10.1016/j.cageo.2011.06.014}.
#'
#'  Santos, F. (2020). Modern methods for old data: An overview of some robust
#'  methods for outliers detection with applications in osteology. *Journal of
#'  Archaeological Science: Reports*, 32, 102423.
#'  \doi{10.1016/j.jasrep.2020.102423}.
#' @example inst/examples/ex-outliers.R
#' @author N. Frerebeau
#' @docType methods
#' @family outlier detection methods
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
#' @param object A [CompositionMatrix-class] object.
#' @param j An [`integer`] giving the index of the rationing part.
#' @param pivot An [`integer`] giving the index of the pivotal variable.
#' @param base A positive [`numeric`] value giving the base with
#'  respect to which logarithms are computed. Change this only if you know
#'  what your are doing.
#' @param ... Currently not used.
#' @return
#'  A [LogRatio-class] object.
#' @references
#'  Aitchison, J. (1986). *The Statistical Analysis of Compositional Data*.
#'  London: Chapman and Hall. \doi{10.1007/978-94-009-4109-0}.
#'
#'  Egozcue, J. J., Pawlowsky-Glahn, V., Mateu-Figueras, G. & Barceló-Vidal, C.
#'  (2003). Isometric Logratio Transformations for Compositional Data Analysis.
#'  *Mathematical Geology*, 35(3), 279-300. \doi{10.1023/A:1023818214614}.
#'
#'  Fišerová, E. & Hron, K. (2011). On the Interpretation of Orthonormal
#'  Coordinates for Compositional Data. *Mathematical Geosciences*, 43(4),
#'  455‑468. \doi{10.1007/s11004-011-9333-x}.
#' @example inst/examples/ex-transform.R
#' @author N. Frerebeau
#' @docType methods
#' @family transformations
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

# Isotopes =====================================================================
#' Geological Model Age from Lead Isotope Analysis
#'
#' Compute geological model age (T) and U/Pb (mu) and Th/U (kappa) ratios from
#' lead isotopic measurements.
#' @param x A [`numeric`] vector of 206Pb/204Pb ratios. If `y` and `z` are
#'  missing, must be a [`list`] (or a [`data.frame`]) with `numeric` components
#'  (columns) `x`, `y` and `z`.
#' @param y A [`numeric`] vector of 207Pb/204Pb ratios.  If missing, an attempt
#'  is made to interpret `x` in a suitable way.
#' @param z A [`numeric`] vector of 208Pb/204Pb ratios.  If missing, an attempt
#'  is made to interpret `x` in a suitable way.
#' @param t0 A [`numeric`] value giving the time of the second stage of the
#'  reference model.
#' @param x_star A [`numeric`] value giving the 206Pb/204Pb ratio at
#'  \eqn{t = 0}.
#' @param y_star A [`numeric`] value giving the 207Pb/204Pb ratio at
#'  \eqn{t = 0}.
#' @param z_star A [`numeric`] value giving the 208Pb/204Pb ratio at
#'  \eqn{t = 0}.
#' @param mu A [`numeric`] value giving the 238U/204Pb ratio of the
#'  reference model.
#' @param kappa A [`numeric`] value giving the 232Th/238U ratio of the
#'  reference model.
#' @param th232 A [`numeric`] value giving the decay constants of 232Th.
#' @param u238 A [`numeric`] value giving the decay constants of 238U.
#' @param u235 A [`numeric`] value giving the decay constants of 235U.
#' @param u238_235 A [`numeric`] value giving the 238U/235U ratio.
#' @param tolerance A [`numeric`] value specifying the tolerance (stopping
#'  criteria for the Newton–Raphson method).
#' @param ... Currently not used.
#' @note
#'  Reference values from Albarede & Juteau (1984).
#' @return
#'  A four columns [`data.frame`]:
#'  \describe{
#'   \item{`age`}{Geological model age (in Ma).}
#'   \item{`mu`}{238U/204Pb ratio.}
#'   \item{`kappa`}{232Th/238U ratio.}
#'   \item{`f`}{}
#'  }
#' @references
#'  Albarède, F., Desaulty, A.-M. & Blichert-Toft, J. (2012). A Geological
#'  Perspective on the Use of Pb Isotopes in Archaeometry. *Archaeometry*, 54:
#'  853-867. \doi{10.1111/j.1475-4754.2011.00653.x}.
#'
#'  Albarède, F. & Juteau, M. (1984). Unscrambling the Lead Model Ages.
#'  *Geochimica et Cosmochimica Acta*, 48(1): 207-12.
#'  \doi{10.1016/0016-7037(84)90364-8}.
#' @example inst/examples/ex-lead.R
#' @author N. Frerebeau, F. Albarede (original Matlab code)
#' @docType methods
#' @family isotope analysis
#' @rdname lia_age
#' @export
setGeneric(
  name = "lia_age",
  def = function(x, y, z, ...) standardGeneric("lia_age"),
  valueClass = "data.frame"
)
