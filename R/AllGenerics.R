# GENERIC METHODS
#' @include AllClasses.R
NULL

# S4 dispatch to base S3 generic ===============================================
setGeneric("dist", package = "stats")
setGeneric("mahalanobis", package = "stats")

# Import S4 generics ===========================================================
#' @importMethodsFrom arkhe describe
#' @importMethodsFrom arkhe replace_NA
#' @importMethodsFrom arkhe replace_zero
#' @importMethodsFrom dimensio pca
NULL

# CoDa =========================================================================
#' Coerce to a Closed Compositional Matrix
#'
#' Coerces an object to a `CompositionMatrix` object.
#' @param from A [`matrix`] or [`data.frame`] to be coerced.
#' @param parts A `vector` giving the index of the column to be used a
#'  compositional parts. If `NULL` (the default), all [`double`] columns will be
#'  used.
#' @param verbose A [`logical`] scalar: should \R report extra information
#'  on progress?
#' @param ... Currently not used.
#' @details
#'  See `vignette("nexus")`.
#' @return A [`CompositionMatrix-class`] object.
#' @example inst/examples/ex-coerce.R
#' @author N. Frerebeau
#' @docType methods
#' @family compositional data tools
#' @aliases as_composition-method
setGeneric(
  name = "as_composition",
  def = function(from, ...) standardGeneric("as_composition"),
  valueClass = "CompositionMatrix"
)

#' Coerce to Amounts
#'
#' @param from A [`CompositionMatrix-class`] object.
#' @param ... Currently not used.
#' @return A [`numeric`] [`matrix`].
#' @example inst/examples/ex-coerce.R
#' @author N. Frerebeau
#' @docType methods
#' @family compositional data tools
#' @aliases as_amounts-method
setGeneric(
  name = "as_amounts",
  def = function(from, ...) standardGeneric("as_amounts"),
  valueClass = "matrix"
)

#' Coerce to Features
#'
#' Converts an object to a collection of features.
#' @param from A [`CompositionMatrix-class`] object.
#' @param ... Currently not used.
#' @return
#'  A [`data.frame`].
#' @example inst/examples/ex-coerce.R
#' @author N. Frerebeau
#' @docType methods
#' @family compositional data tools
#' @aliases as_features-method
setGeneric(
  name = "as_features",
  def = function(from, ...) standardGeneric("as_features")
)

#' Data Description
#'
#' Describes an object.
#' @param x A [`CompositionMatrix-class`] object.
#' @return
#'  `describe()` is called for its side-effects. Invisibly returns `x`.
#' @example inst/examples/ex-describe.R
#' @author N. Frerebeau
#' @docType methods
#' @family data summaries
#' @name describe
#' @rdname describe
NULL

# Simplex ======================================================================
#' Operations in the Simplex
#'
#' Operators performing operations in the simplex.
#' @param x A [`CompositionMatrix-class`] object.
#' @param y A [`CompositionMatrix-class`] object or a [`numeric`] vector.
#' @details
#'  \describe{
#'   \item{`%perturbe%`}{[Perturbation operation][perturbation()].}
#'   \item{`%power%`}{[Powering operation][powering()].}
#'  }
#' @return
#'  A [`CompositionMatrix-class`] object or a [`numeric`] vector (same as `x`).
#' @example inst/examples/ex-arith.R
#' @author N. Frerebeau
#' @docType methods
#' @family operations in the simplex
#' @name arithmetic
#' @rdname arithmetic
NULL

#' @rdname arithmetic
#' @aliases `%perturbe%`-method
setGeneric(
  name = "%perturbe%",
  def = function(x, y) standardGeneric("%perturbe%")
)

#' @rdname arithmetic
#' @aliases `%power%`-method
setGeneric(
  name = "%power%",
  def = function(x, y) standardGeneric("%power%")
)

#' Closure Operation
#'
#' Closes compositions to sum up to 1.
#' @param x A [`numeric`] vector or matrix.
#' @param total A [numeric] vector specifying the total amount to which the
#'  compositions should be closed (defaults to 1).
#' @param na.rm A [`logical`] scalar: should missing values be removed?
#' @param ... Currently not used.
#' @return A [`numeric`] vector or matrix (same as `x`).
#' @example inst/examples/ex-arith.R
#' @author N. Frerebeau
#' @docType methods
#' @family operations in the simplex
#' @aliases closure-method
setGeneric(
  name = "closure",
  def = function(x, ...) standardGeneric("closure")
)

#' Perturbation Operation
#'
#' Perturbation of two compositions.
#' @param x,y A [`numeric`] vector of compositional data or a
#'  [`CompositionMatrix-class`] object.
#' @param ... Currently not used.
#' @details
#'  In compositional geometry, perturbation plays the role of sum (translation).
#'  It is the closed component-wise product of two compositions.
#' @return A [`numeric`] vector.
#' @example inst/examples/ex-arith.R
#' @author N. Frerebeau
#' @docType methods
#' @family operations in the simplex
#' @aliases perturbation-method
setGeneric(
  name = "perturbation",
  def = function(x, y, ...) standardGeneric("perturbation")
)

#' Powering Operation
#'
#' Perturbation of two compositions.
#' @param x A [`numeric`] vector of compositional data or a
#'  [`CompositionMatrix-class`] object.
#' @param a A [`numeric`] constant.
#' @param ... Currently not used.
#' @details
#'  In compositional geometry, powering replaces the product of a vector by a
#'  scalar (scaling) and is defined as the closed powering of the components by
#'  a given scalar.
#' @return A [`numeric`] vector.
#' @example inst/examples/ex-arith.R
#' @author N. Frerebeau
#' @docType methods
#' @family operations in the simplex
#' @aliases powering-method
setGeneric(
  name = "powering",
  def = function(x, a, ...) standardGeneric("powering")
)

#' Scalar Product
#'
#' Computes the Aitchison scalar product of two compositions.
#' @param x,y A [`CompositionMatrix-class`] object.
#' @param ... Currently not used.
#' @return A [`numeric`] vector.
#' @example inst/examples/ex-arith.R
#' @author N. Frerebeau
#' @docType methods
#' @family operations in the simplex
#' @aliases scalar-method
setGeneric(
  name = "scalar",
  def = function(x, y, ...) standardGeneric("scalar")
)

# Extract ======================================================================
## Mutators --------------------------------------------------------------------
#' Working With Groups
#'
#' Retrieves or defines the groups to which the observations belong.
#' @param x An object from which to get or set `groups`.
# @param value A possible value for the `groups` of `x`.
#' @return
#'  * `get_groups()` returns a [`character`] vector giving the group names of `x`.
#' @example inst/examples/ex-mutators.R
#' @author N. Frerebeau
#' @docType methods
#' @family mutators
#' @name groups
#' @rdname groups
NULL

#' @rdname groups
#' @aliases get_groups-method
setGeneric(
  name = "get_groups",
  def = function(x) standardGeneric("get_groups")
)

#' Row Sums
#'
#' Retrieves or defines the row sums (before closure).
#' @param x An object from which to get or set `totals`.
#' @param value A possible value for the `totals` of `x`.
#' @return
#'  * `set_totals()` returns an object of the same sort as `x` with the new
#'    row sums assigned.
#'  * `get_totals()` returns the row sums of `x`.
#' @example inst/examples/ex-coerce.R
#' @author N. Frerebeau
#' @docType methods
#' @family mutators
#' @name totals
#' @rdname totals
NULL

#' @rdname totals
#' @aliases get_totals-method
setGeneric(
  name = "get_totals",
  def = function(x) standardGeneric("get_totals")
)

#' @rdname totals
#' @aliases set_totals-method
setGeneric(
  name = "set_totals<-",
  def = function(x, value) standardGeneric("set_totals<-")
)

## Subset ----------------------------------------------------------------------
#' Extract or Replace Parts of an Object
#'
#' Operators acting on objects to extract or replace parts.
#' @param x An object from which to extract element(s) or in which to replace
#'  element(s).
#' @param i,j Indices specifying elements to extract or replace. Indices are
#'  [`numeric`], [`integer`] or [`character`] vectors or empty (missing) or
#'  `NULL`. Numeric values are coerced to [`integer`] as by [as.integer()].
#'  Character vectors will be matched to the name of the elements.
#'  An empty index (a comma separated blank) indicates that all entries in that
#'  dimension are selected.
#' @param value A possible value for the element(s) of `x`.
#' @param drop A [`logical`] scalar: should the result be coerced to
#'  the lowest possible dimension? This only works for extracting elements,
#'  not for the replacement. Defaults to `FALSE`.
#' @param ... Currently not used.
# @section Subcomposition:
#  If `drop` is `FALSE`, subsetting some of the possible components of a
#  [`CompositionMatrix-class`] object will produce a closed *subcomposition*
#  (see examples).
#' @return
#'  A subsetted object of the same sort as `x`.
#' @example inst/examples/ex-subset.R
#' @author N. Frerebeau
#' @docType methods
#' @family mutators
#' @name subset
#' @rdname subset
NULL

#' Divide into Groups
#'
#' Divides the compositional matrix `x` into the groups defined by `f`.
#' @param x A [`CompositionMatrix-class`] object.
#' @param f A 'factor' in the sense that [`as.factor(f)`][as.factor()] defines
#'  the grouping, or a list of such factors in which case their interaction is
#'  used for the grouping (see [base::split()]).
#' @param drop A [`logical`] scalar: should levels that do not occur be dropped?
#' @param ... Currently not used.
#' @return
#'  A `list` of [`CompositionMatrix-class`] objects.
#' @example inst/examples/ex-split.R
#' @author N. Frerebeau
#' @docType methods
#' @family mutators
#' @name split
#' @rdname split
NULL

#' Matrix Transpose
#'
#' @param x A [`CompositionMatrix-class`] object.
#' @return
#'  A `matrix`, with dim and dimnames constructed appropriately from those of `x`.
#' @note
#'  Implemented only to ensure that `t()` always returns a base `matrix`.
#' @example inst/examples/ex-subset.R
#' @author N. Frerebeau
#' @docType methods
# @family mutators
#' @keywords internal
#' @name t
#' @rdname t
NULL

# Log-Ratio ====================================================================
## LR --------------------------------------------------------------------------
#' Pairwise Log-Ratios (LR)
#'
#' Computes all pairwise log-ratio transformation.
#' @param object A [`CompositionMatrix-class`] object.
#' @param ... Currently not used.
#' @return
#'  A [`LR-class`] object.
#' @references
#'  Aitchison, J. (1986). *The Statistical Analysis of Compositional Data*.
#'  London: Chapman and Hall.
#'
#'  Greenacre, M. J. (2019). *Compositional Data Analysis in Practice*.
#'  Boca Raton: CRC Press.
#'
#'  Greenacre, M. J. (2021). Compositional Data Analysis. *Annual Review of
#'  Statistics and Its Application*, 8(1): 271-299.
#'  \doi{10.1146/annurev-statistics-042720-124436}.
#' @example inst/examples/ex-transform-lr.R
#' @author N. Frerebeau
#' @docType methods
#' @family log-ratio transformations
#' @aliases transform_lr-method
setGeneric(
  name = "transform_lr",
  def = function(object, ...) standardGeneric("transform_lr"),
  valueClass = "LR"
)

## CLR -------------------------------------------------------------------------
#' Centered Log-Ratios (CLR)
#'
#' Computes CLR transformation.
#' @param object A [`CompositionMatrix-class`] object.
#' @param weights A [`logical`] scalar: sould a varying weight be used. If
#'  `FALSE` (the default), equally-weighted parts are used. Alternatively, a
#'  positive [`numeric`] vector of weights can be specified.
#' @param ... Currently not used.
#' @details
#'  The CLR transformation computes the log of each part relative to the
#'  geometric mean of all parts.
#' @return
#'  A [`CLR-class`] object.
#' @references
#'  Aitchison, J. (1986). *The Statistical Analysis of Compositional Data*.
#'  London: Chapman and Hall.
#'
#'  Greenacre, M. J. (2019). *Compositional Data Analysis in Practice*.
#'  Boca Raton: CRC Press.
#'
#'  Greenacre, M. J. (2021). Compositional Data Analysis. *Annual Review of
#'  Statistics and Its Application*, 8(1): 271-299.
#'  \doi{10.1146/annurev-statistics-042720-124436}.
#' @example inst/examples/ex-transform-clr.R
#' @author N. Frerebeau
#' @docType methods
#' @family log-ratio transformations
#' @aliases transform_clr-method
setGeneric(
  name = "transform_clr",
  def = function(object, ...) standardGeneric("transform_clr"),
  valueClass = "CLR"
)

## ALR -------------------------------------------------------------------------
#' Additive Log-Ratios (ALR)
#'
#' Computes ALR transformation.
#' @param object A [`CompositionMatrix-class`] object.
#' @param j An [`integer`] giving the index of the rationing part (denominator).
#' @param ... Currently not used.
#' @details
#'  The ALR transformation is the logratio of a pair of parts with respect to a
#'  fixed part.
#' @return
#'  An [`ALR-class`] object.
#' @references
#'  Aitchison, J. (1986). *The Statistical Analysis of Compositional Data*.
#'  London: Chapman and Hall.
#'
#'  Greenacre, M. J. (2019). *Compositional Data Analysis in Practice*.
#'  Boca Raton: CRC Press.
#'
#'  Greenacre, M. J. (2021). Compositional Data Analysis. *Annual Review of
#'  Statistics and Its Application*, 8(1): 271-299.
#'  \doi{10.1146/annurev-statistics-042720-124436}.
#' @example inst/examples/ex-transform-alr.R
#' @author N. Frerebeau
#' @docType methods
#' @family log-ratio transformations
#' @aliases transform_alr-method
setGeneric(
  name = "transform_alr",
  def = function(object, ...) standardGeneric("transform_alr"),
  valueClass = "ALR"
)

## ILR -------------------------------------------------------------------------
#' Isometric Log-Ratios (ILR)
#'
#' Computes ILR transformations.
#' @param object A [`CompositionMatrix-class`] object.
#' @param base A [`matrix`] giving the base of the transformation.
#' @param ... Currently not used.
#' @details
#'  The ILR transformation provides the coordinates of any composition with
#'  respect to a given orthonormal basis. `transform_ilr()` uses the orthonormal
#'  basis (Helmert matrix) originally defined by Egozcue *et al.* (2003).
#' @return
#'  An [`ILR-class`] object.
#' @references
#'  Egozcue, J. J., Pawlowsky-Glahn, V., Mateu-Figueras, G. & Barceló-Vidal, C.
#'  (2003). Isometric Logratio Transformations for Compositional Data Analysis.
#'  *Mathematical Geology*, 35(3), 279-300. \doi{10.1023/A:1023818214614}.
#'
#'  Greenacre, M. J. (2019). *Compositional Data Analysis in Practice*.
#'  Boca Raton: CRC Press.
#'
#'  Greenacre, M. J. (2021). Compositional Data Analysis. *Annual Review of
#'  Statistics and Its Application*, 8(1): 271-299.
#'  \doi{10.1146/annurev-statistics-042720-124436}.
#' @example inst/examples/ex-transform-ilr.R
#' @author N. Frerebeau
#' @docType methods
#' @family log-ratio transformations
#' @aliases transform_ilr-method
setGeneric(
  name = "transform_ilr",
  def = function(object, base, ...) standardGeneric("transform_ilr"),
  valueClass = "ILR"
)

#' Univariate Isometric Log-Ratios (ILR)
#'
#' Computes univariate ILR coordinates.
#' @param object A [`CompositionMatrix-class`] object.
#' @param ... Currently not used.
#' @details
#'  The ILR transformation provides the coordinates of any composition with
#'  respect to a given orthonormal basis. `transform_ilr()` uses the orthonormal
#'  basis (Helmert matrix) originally defined by Egozcue *et al.* (2003).
#' @return
#'  Same as `object`.
#' @references
#'  Filzmoser, P., Hron, K. & Reimann, C. (2009). Univariate Statistical
#'  Analysis of Environmental (Compositional) Data: Problems and Possibilities.
#'  *Science of The Total Environment*, 407(23), 6100-6108.
#'  \doi{10.1016/j.scitotenv.2009.08.008}.
#'
#'  Filzmoser, P., Hron, K. & Reimann, C. (2010). The Bivariate Statistical
#'  Analysis of Environmental (Compositional) Data. *Science of The Total
#'  Environment*, 408(19), 4230-4238. \doi{10.1016/j.scitotenv.2010.05.011}.
#' @example inst/examples/ex-transform-ilr.R
#' @author N. Frerebeau
#' @docType methods
# @family log-ratio transformations
#' @aliases univariate_ilr-method
#' @keywords internal
setGeneric(
  name = "univariate_ilr",
  def = function(object, ...) standardGeneric("univariate_ilr")
)

## PLR -------------------------------------------------------------------------
#' Pivot Log-Ratios (PLR)
#'
#' Computes PLR transformations.
#' @param object A [`CompositionMatrix-class`] object.
#' @param pivot An [`integer`] giving the index of the pivotal variable.
#' @param ... Currently not used.
#' @return
#'  A [`PLR-class`] object.
#' @references
#'  Fišerová, E. & Hron, K. (2011). On the Interpretation of Orthonormal
#'  Coordinates for Compositional Data. *Mathematical Geosciences*, 43(4),
#'  455‑468. \doi{10.1007/s11004-011-9333-x}.
#'
#'  Greenacre, M. J. (2019). *Compositional Data Analysis in Practice*.
#'  Boca Raton: CRC Press.
#'
#'  Greenacre, M. J. (2021). Compositional Data Analysis. *Annual Review of
#'  Statistics and Its Application*, 8(1): 271-299.
#'  \doi{10.1146/annurev-statistics-042720-124436}.
#'
#'  Hron, K., Filzmoser, P., de Caritat, P., Fišerová, E. & Gardlo, A. (2017).
#'  Weighted Pivot Coordinates for Compositional Data and Their Application to
#'  Geochemical Mapping. *Mathematical Geosciences*, 49(6), 797-814.
#'  \doi{10.1007/s11004-017-9684-z}.
#' @example inst/examples/ex-transform-ilr.R
#' @author N. Frerebeau
#' @docType methods
#' @family log-ratio transformations
#' @aliases transform_plr-method
setGeneric(
  name = "transform_plr",
  def = function(object, ...) standardGeneric("transform_plr"),
  valueClass = "PLR"
)

## Inverse ---------------------------------------------------------------------
#' Inverse Log-Ratio Transformation
#'
#' Computes inverse log-ratio transformations.
#' @param object A [`LogRatio-class`] object.
#' @param origin A [`LogRatio-class`] object to be used for the inverse
#'  transformation.
#' @param ... Currently not used.
#' @return
#'  A [`CompositionMatrix-class`] object.
#' @references
#'  Aitchison, J. (1986). *The Statistical Analysis of Compositional Data*.
#'  London: Chapman and Hall.
#'
#'  Egozcue, J. J., Pawlowsky-Glahn, V., Mateu-Figueras, G. & Barceló-Vidal, C.
#'  (2003). Isometric Logratio Transformations for Compositional Data Analysis.
#'  *Mathematical Geology*, 35(3), 279-300. \doi{10.1023/A:1023818214614}.
#'
#'  Fišerová, E. & Hron, K. (2011). On the Interpretation of Orthonormal
#'  Coordinates for Compositional Data. *Mathematical Geosciences*, 43(4),
#'  455‑468. \doi{10.1007/s11004-011-9333-x}.
#'
#'  Greenacre, M. J. (2019). *Compositional Data Analysis in Practice*.
#'  Boca Raton: CRC Press.
#' @example inst/examples/ex-transform-clr.R
#' @author N. Frerebeau
#' @docType methods
#' @family log-ratio transformations
#' @aliases transform_inverse-method
setGeneric(
  name = "transform_inverse",
  def = function(object, origin, ...) standardGeneric("transform_inverse"),
  valueClass = "matrix"
)

# Statistics ===================================================================
#' Compute Summary Statistics of Data Subsets
#'
#' Splits the data into subsets, computes summary statistics for each, and
#' returns the result.
#' @param x A [`CompositionMatrix-class`] object.
#' @param by A `vector` or a list of grouping elements, each as long as the
#'  variables in `x`. The elements are coerced to factors before use
#'  (in the sense that [`as.factor(by)`][as.factor()] defines the grouping).
#' @param FUN A [`function`] to compute the summary statistics.
#' @param simplify A [`logical`] scalar: should the results be simplified to a
#'  matrix if possible?
#' @param drop A [`logical`] scalar indicating whether to drop unused
#'  combinations of grouping values.
#' @param ... Further arguments to be passed to `FUN`.
#' @return A [`matrix`].
#' @example inst/examples/ex-aggregate.R
#' @author N. Frerebeau
#' @docType methods
#' @family statistics
#' @name aggregate
#' @rdname aggregate
NULL

#' Compositional Mean
#'
#' @param x A [`CompositionMatrix-class`] object.
#' @param ... Further arguments to be passed to internal methods.
#' @details
#'  Closed vector of the columns geometric means.
#' @return A [`numeric`] vector.
#' @references
#'  Aitchison, J. (1986). *The Statistical Analysis of Compositional Data*.
#'  London: Chapman and Hall, p. 64-91.
#' @example inst/examples/ex-mean.R
#' @author N. Frerebeau
#' @docType methods
#' @family statistics
#' @name mean
#' @rdname mean
NULL

#' Sample Quantiles
#'
#' @param x A [`CompositionMatrix-class`] object.
#' @param probs A [`numeric`] vector of probabilities with values in \eqn{[0,1]}.
#' @param na.rm A [`logical`] scalar: should missing values be removed?
#' @param names A [`logical`] scalar: should results be named?
#' @param ... Currently not used.
#' @return A [`numeric`] matrix.
#' @references
#'  Filzmoser, P., Hron, K. & Reimann, C. (2009). Univariate Statistical
#'  Analysis of Environmental (Compositional) Data: Problems and Possibilities.
#'  *Science of The Total Environment*, 407(23): 6100-6108.
#'  \doi{10.1016/j.scitotenv.2009.08.008}.
#' @example inst/examples/ex-mean.R
#' @author N. Frerebeau
#' @docType methods
#' @family statistics
#' @name quantile
#' @rdname quantile
NULL

#' Compositional Mean of Data Subsets
#'
#' Splits the data into subsets and computes compositional mean for each.
#' @param x A [`CompositionMatrix-class`] object.
#' @param by A `vector` of grouping elements, as long as the variables in `x`
#'  (in the sense that [`as.factor(by)`][as.factor()] defines the grouping).
#'  If a single `character` string is passed, it must be the name of a
#'  categorical variable from the original dataset.
#' @param ... Further arguments to be passed to [mean()].
#' @return A [`CompositionMatrix-class`] object.
#' @seealso [mean()], [aggregate()]
#' @example inst/examples/ex-condense.R
#' @author N. Frerebeau
#' @docType methods
#' @family statistics
#' @aliases condense-method
setGeneric(
  name = "condense",
  def = function(x, ...) standardGeneric("condense"),
  valueClass = "CompositionMatrix"
)

#' Marginal Compositions
#'
#' @param x A [`CompositionMatrix-class`] object.
#' @param parts An [`integer`] or a [`character`] vector specifying the columns
#'  to be selected.
#' @param name A [`character`] string giving the name of the amalgamation
#'  column.
#' @param ... Currently not used.
#' @return A [`CompositionMatrix-class`] object.
#' @example inst/examples/ex-margin.R
#' @author N. Frerebeau
#' @docType methods
#' @family statistics
#' @aliases margin-method
setGeneric(
  name = "margin",
  def = function(x, ...) standardGeneric("margin"),
  valueClass = "CompositionMatrix"
)

#' Metric Variance and Standard Deviation
#'
#' @description
#'  * `metric_var()` computes the metric variance (or total variance), i.e. a
#'    global measure of spread.
#'  * `metric_sd()` computes the metric standard deviation.
#' @param x A [`CompositionMatrix-class`] object.
#' @param ... Currently not used.
#' @return A [`numeric`] vector.
#' @details
#'  The metric variance is the average of the [CLR][transform_clr()] variances.
#' @references
#'  Boogaart, K. G. van den & Tolosana-Delgado, R. (2013). *Analyzing
#'  Compositional Data with R*. Berlin Heidelberg: Springer-Verlag.
#'  \doi{10.1007/978-3-642-36809-7}.
#'
#'  Hron, K. & Kubáček. L. (2011). Statistical Properties of the Total Variation
#'  Estimator for Compositional Data. *Metrika*, 74 (2): 221-230.
#'  \doi{10.1007/s00184-010-0299-3}.
#'
#'  Pawlowsky-Glahn, V. & Egozcue, J. J. (2001). Geometric Approach to
#'  Statistical Analysis on the Simplex. *Stochastic Environmental Research and
#'  Risk Assessment*, 15(5): 384-398. \doi{10.1007/s004770100077}.
#' @example inst/examples/ex-mean.R
#' @author N. Frerebeau
#' @docType methods
#' @family statistics
#' @aliases metric_var-method
setGeneric(
  name = "metric_var",
  def = function(x, ...) standardGeneric("metric_var")
)

#' @rdname metric_var
#' @aliases metric_sd-method
setGeneric(
  name = "metric_sd",
  def = function(x, ...) standardGeneric("metric_sd")
)

#' Covariance Matrix
#'
#' Computes the (centered) log-ratio covariance matrix (see below).
#' @param x A [`CompositionMatrix-class`] object.
#' @param center A [`logical`] scalar: should the *centered* log-ratio
#'  covariance matrix be computed?
#' @param method A [`character`] string indicating which covariance is to be
#'  computed (see [stats::cov()]).
#' @param ... Currently not used.
#' @return A [`matrix`].
#' @references
#'  Aitchison, J. (1986). *The Statistical Analysis of Compositional Data*.
#'  London: Chapman and Hall, p. 64-91.
#'
#'  Greenacre, M. J. (2019). *Compositional Data Analysis in Practice*.
#'  Boca Raton: CRC Press.
#' @example inst/examples/ex-covariance.R
#' @author N. Frerebeau
#' @docType methods
#' @family statistics
#' @aliases covariance-method
setGeneric(
  name = "covariance",
  def = function(x, ...) standardGeneric("covariance"),
  valueClass = "matrix"
)

#' Variation Matrix
#'
#' Computes the variation matrix (Aitchison 1986, definition 4.4).
#' @param x A [`CompositionMatrix-class`] object.
#' @param ... Currently not used.
#' @return A [`matrix`].
#' @references
#'  Aitchison, J. (1986). *The Statistical Analysis of Compositional Data*.
#'  London: Chapman and Hall, p. 64-91.
#'
#'  Greenacre, M. J. (2019). *Compositional Data Analysis in Practice*.
#'  Boca Raton: CRC Press.
#' @example inst/examples/ex-variation.R
#' @author N. Frerebeau
#' @docType methods
#' @family statistics
#' @aliases variation-method
setGeneric(
  name = "variation",
  def = function(x, ...) standardGeneric("variation"),
  valueClass = "matrix"
)

#' Proportionality Index of Parts (PIP)
#'
#' Computes an index of association between parts.
#' @param x A [`CompositionMatrix-class`] object.
#' @param ... Currently not used.
#' @return A [`matrix`].
#' @details
#'  The proportionality index of parts (PIP) is based on the
#'  [variation matrix][variation()], but maintains the range of values whithin
#'  \eqn{(0,1)}.
#' @references
#'  Egozcue, J. J.. & Pawlowsky-Glahn, V. (2023). Subcompositional Coherence
#'  and and a Novel Proportionality Index of Parts. *SORT*, 47(2): 229-244.
#'  \doi{10.57645/20.8080.02.7}.
#' @example inst/examples/ex-variation.R
#' @author N. Frerebeau
#' @docType methods
#' @family statistics
#' @aliases pip-method
setGeneric(
  name = "pip",
  def = function(x, ...) standardGeneric("pip"),
  valueClass = "matrix"
)

# Variation Array
#
# Computes the compositional variation array.
# @param object A [`CompositionMatrix-class`] object.
# @param ... Currently not used.
# @return A [`matrix`].
# @details
#  The compositional variation array is a square matrix where the upper
#  triangular part displays the pairwise log-ratio variances and the lower
#  triangular part displays the pairwise log-ratio means.
# @references
#  Aitchison, J. (1986). *The Statistical Analysis of Compositional Data*.
#  London: Chapman and Hall, p. 64-91.
# @example inst/examples/ex-variation_array.R
# @author N. Frerebeau
# @docType methods
# @family statistics
# @aliases variation_array-method
# setGeneric(
#   name = "variation_array",
#   def = function(object, ...) standardGeneric("variation_array"),
#   valueClass = "matrix"
# )

#' Scaling and Centering of Compositional Data
#'
#' @param x A [`CompositionMatrix-class`] object.
#' @param center A [`logical`] scalar or a [`numeric`] vector giving the center
#'  to be substracted.
#' @param scale A [`logical`] scalar or a length-one [`numeric`] vector giving a
#'  scaling factor for multiplication.
#' @return A [`CompositionMatrix-class`] object.
#' @references
#'  Aitchison, J. (1986). *The Statistical Analysis of Compositional Data*.
#'  London: Chapman and Hall, p. 64-91.
#'
#'  Boogaart, K. G. van den & Tolosana-Delgado, R. (2013). *Analyzing
#'  Compositional Data with R*. Berlin Heidelberg: Springer-Verlag.
#'  \doi{10.1007/978-3-642-36809-7}.
#' @example inst/examples/ex-scale.R
#' @author N. Frerebeau
#' @docType methods
#' @family statistics
#' @name scale
#' @rdname scale
NULL

# Distances ====================================================================
#' Distances
#'
#' Computes the log-ratio variance matrix.
#' @param x A [`CompositionMatrix-class`] object.
#' @param method A [`character`] string specifying the distance measure to be
#'  used. See [stats::dist()] for the available distances.
#' @param diag A [`logical`] scalar indicating whether the diagonal of the
#'  distance matrix should be printed.
#' @param upper A [`logical`] scalar indicating whether the upper triangle of
#'  the distance matrix should be printed.
#' @param p An [`integer`] giving the power of the Minkowski distance.
#' @details
#'  Distances are computed on [CLR-transformed][transform_clr] data.
#' @return A [`stats::dist`] object.
#' @references
#'  Aitchison, J. (1986). *The Statistical Analysis of Compositional Data*.
#'  London: Chapman and Hall, p. 64-91.
#'
#'  Greenacre, M. J. (2019). *Compositional Data Analysis in Practice*.
#'  Boca Raton: CRC Press.
#' @example inst/examples/ex-dist.R
#' @seealso [stats::dist()]
#' @author N. Frerebeau
#' @docType methods
#' @family statistics
#' @name dist
#' @rdname dist
NULL

#' Mahalanobis Distance
#'
#' Computes the squared Mahalanobis distance of all rows in `x`.
#' @param x A [`CompositionMatrix-class`] or an [`ILR-class`] object.
#' @param center A [`numeric`] vector giving the mean vector of the
#'  distribution. If missing, will be estimated from `x`.
#' @param cov A [`numeric`] matrix giving the covariance of the
#'  distribution. If missing, will be estimated from `x`.
#' @param robust A [`logical`] scalar: should robust location and scatter
#'  estimation be used?
#' @param method A [`character`] string specifying the method to be used.
#'  It must be one of "`mve`" (minimum volume ellipsoid) or "`mcd`" (minimum
#'  covariance determinant). Only used if `robust` is `TRUE`.
#' @param ... Extra parameters to be passed to [MASS::cov.rob()].
#'  Only used if `robust` is `TRUE`.
#' @return A [`numeric`] vector.
#' @example inst/examples/ex-mahalanobis.R
#' @seealso [stats::mahalanobis()]
#' @author N. Frerebeau
#' @docType methods
#' @family statistics
#' @name mahalanobis
#' @rdname mahalanobis
NULL

# Plot =========================================================================
## Barplot ---------------------------------------------------------------------
#' Barplot of Compositional Data
#'
#' Displays a compositional bar chart.
#' @param height A [`CompositionMatrix-class`] object.
#' @param groups A `vector` of grouping elements, as long as the variables in
#'  `height`. If a single `character` string is passed, it must be the name of a
#'  categorical variable from the original dataset.
#'  If set, a matrix of panels defined by `groups` will be drawn.
#' @param order An [`integer`] vector giving the index of the column to be used
#'  for the ordering of the data.
#' @param decreasing A [`logical`] scalar: should the sort order be increasing
#'  or decreasing?
#' @param horiz A [`logical`] scalar. If `FALSE`, the bars are drawn vertically
#'  with the first bar to the left. If `TRUE` (the default), the bars are drawn
#'  horizontally with the first at the bottom.
#' @param xlab,ylab A [`character`] vector giving the x and y axis labels.
#' @param main A [`character`] string giving a main title for the plot.
#' @param sub A [`character`] string giving a subtitle for the plot.
#' @param ann A [`logical`] scalar: should the default annotation (title and x
#'  and y axis labels) appear on the plot?
#' @param axes A [`logical`] scalar: should axes be drawn on the plot?
#' @param col A vector of colors for the bar components.
#' @param legend A [`list`] of additional arguments to be passed to
#'  [graphics::legend()]; names of the list are used as argument names.
#'  If `NULL`, no legend is displayed.
#' @param ... Further parameters to be passed to [graphics::barplot()].
#' @return
#'  `barplot()` is called for its side-effects: is results in a graphic being
#'  displayed (invisibly return `height`).
#' @example inst/examples/ex-barplot.R
#' @author N. Frerebeau
#' @docType methods
#' @family plot methods
#' @name barplot
#' @rdname barplot
NULL

## Histogram -------------------------------------------------------------------
#' Histogram of Compositional Data
#'
#' Produces an histogram of univariate ILR data (see Filzmoser *et al.*, 2009).
#' @param x A [`CompositionMatrix-class`] object.
#' @param freq A [`logical`] scalar: should absolute frequencies (counts) be
#'  displayed? If `FALSE` (the default), relative frequencies (probabilities)
#'  are displayed (see [graphics::hist()]).
#' @param flip A [`logical`] scalar: should the y-axis (ticks and numbering) be
#'  flipped from side 2 (left) to 4 (right) from variable to variable?
#' @param ncol An [`integer`] specifying the number of columns to use.
#'  Defaults to 1 for up to 4 parts, otherwise to 2.
#' @param main A [`character`] string giving a main title for the plot.
#' @param sub A [`character`] string giving a subtitle for the plot.
#' @param ann A [`logical`] scalar: should the default annotation (title and x
#'  and y axis labels) appear on the plot?
#' @param axes A [`logical`] scalar: should axes be drawn on the plot?
#' @param frame.plot A [`logical`] scalar: should a box be drawn around the
#'  plot?
#' @param ... Further parameters to be passed to [graphics::hist()].
#' @return
#'  `hist()` is called for its side-effects: is results in a graphic being
#'  displayed (invisibly return `x`).
#' @references
#'  Filzmoser, P., Hron, K. & Reimann, C. (2009). Univariate Statistical
#'  Analysis of Environmental (Compositional) Data: Problems and Possibilities.
#'  *Science of The Total Environment*, 407(23): 6100-6108.
#'  \doi{10.1016/j.scitotenv.2009.08.008}.
#' @example inst/examples/ex-hist.R
#' @author N. Frerebeau
#' @docType methods
#' @family plot methods
#' @name hist
#' @rdname hist
NULL

## Ternary ---------------------------------------------------------------------
#' Plot Compositional Data
#'
#' Displays a matrix of ternary plots.
#' @param x A [`CompositionMatrix-class`] object.
#' @param groups A `vector` of grouping elements, as long as the variables in
#'  `x`. If a single `character` string is passed, it must be the name of a
#'  categorical variable from the original dataset.
#' @inheritParams isopleuros::ternary_pairs
#' @return
#'  `plot()` is called for its side-effects: is results in a graphic being
#'  displayed (invisibly return `x`).
#' @seealso [isopleuros::ternary_pairs()], [isopleuros::ternary_plot()]
#' @example inst/examples/ex-plot.R
#' @author N. Frerebeau
#' @docType methods
#' @family plot methods
#' @name plot
#' @rdname plot
NULL

## Density ---------------------------------------------------------------------
#' Plot Log-Ratios
#'
#' Displays a density plot.
#' @param x A [`LogRatio-class`] object.
#' @param groups A `vector` of grouping elements, as long as the variables in
#'  `x`. If a single `character` string is passed, it must be the name of a
#'  categorical variable from the original dataset.
#'  If set, a matrix of panels defined by `groups` will be drawn.
#' @param rug A [`logical`] scalar: should a *rug* representation (1-d plot) of
#'  the data be added to the plot?
#' @param ticksize A length-one [`numeric`] vector giving the length of the
#'  ticks making up the *rug*. Positive lengths give inwards ticks. Only used if
#'  `rug` is `TRUE`.
#' @param flip A [`logical`] scalar: should the y-axis (ticks and numbering) be
#'  flipped from side 2 (left) to 4 (right) from variable to variable?
#' @param ncol An [`integer`] specifying the number of columns to use.
#'  Defaults to 1 for up to 4 groups, otherwise to 2.
#' @param xlab,ylab A [`character`] vector giving the x and y axis labels.
#' @param main A [`character`] string giving a main title for the plot.
#' @param ann A [`logical`] scalar: should the default annotation (title and x
#'  and y axis labels) appear on the plot?
#' @param axes A [`logical`] scalar: should axes be drawn on the plot?
#' @param frame.plot A [`logical`] scalar: should a box be drawn around the
#'  plot?
#' @param legend A [`list`] of additional arguments to be passed to
#'  [graphics::legend()]; names of the list are used as argument names.
#'  If `NULL`, no legend is displayed.
#' @param ... Further [graphical parameters][graphics::par()], particularly,
#'  `border` and `col`.
#' @return
#'  `plot()` is called for its side-effects: is results in a graphic being
#'  displayed (invisibly return `x`).
#' @example inst/examples/ex-density.R
#' @author N. Frerebeau
#' @docType methods
#' @family plot methods
#' @name plot_logratio
#' @rdname plot_logratio
NULL

## Graph -----------------------------------------------------------------------
#' Graph of Log-ratios
#'
#' Produces a graph of log-ratios.
#' @param object A [`LogRatio-class`] object.
#' @param ... Currently not used.
#' @return
#'  An \pkg{igraph} graph object.
#' @example inst/examples/ex-graph.R
#' @author N. Frerebeau
#' @docType methods
#' @family plot methods
#' @aliases as_graph-method
setGeneric(
  name = "as_graph",
  def = function(object, ...) standardGeneric("as_graph")
)

# PCA ==========================================================================
#' Principal Components Analysis
#'
#' Computes a principal components analysis based on the singular value
#' decomposition.
#' @param object A [`LogRatio-class`] object.
#' @inheritParams dimensio::pca
#' @return
#'  A [`dimensio::PCA-class`] object. See package \pkg{dimensio} for details.
#' @references
#'  Aitchison, J. and Greenacre, M. (2002). Biplots of compositional data.
#'  *Journal of the Royal Statistical Society: Series C (Applied Statistics)*,
#'  51: 375-392. \doi{10.1111/1467-9876.00275}.
#'
#'  Filzmoser, P., Hron, K. and Reimann, C. (2009). Principal component analysis
#'  for compositional data with outliers. *Environmetrics*, 20: 621-632.
#'  \doi{10.1002/env.966}.
#' @example inst/examples/ex-pca.R
#' @seealso [dimensio::pca()], [dimensio::biplot()], [dimensio::screeplot()],
#'  [dimensio::viz_individuals()], [dimensio::viz_variables()]
#' @author N. Frerebeau
#' @docType methods
#' @family multivariate analysis
#' @name pca_coda
#' @rdname pca
NULL

# Missign Values ===============================================================
#' Missing Values Policy
#'
#' @details
#'  Compositional data are quantitative positive descriptions of the parts
#'  of some whole, carrying relative, rather than absolute, information
#'  (ie. only relative changes are relevant; Aitchison 1986).
#'
#'  Basically, three situations can be outlined regarding missing values in
#'  compositions:
#'
#'  * Unobserved quantities.
#'  * Amounts observed, but which happen to be below the detection limit
#'    (thus interpreted as small unknown values).
#'  * Absolutely zero quantities.
#'
#' These situations can be represented in several ways:
#'
#'  * The presence of zeros.
#'  * The presence of missing values (`NA`).
#'
#'  When creating a [`CompositionMatrix-class`] object, the presence of zero
#'  and [`NA`] values is allowed: this makes it possible to explore and
#'  visualize the data while preserving the missing structure. However, **the
#'  user must deal with these missing values before proceeding further** (e.g.
#'  by removing incomplete cases or replacing the values concerned): log-ratio
#'  transformations cannot be computed in the presence of zeros or missing
#'  values.
#' @note
#'  If you need more advanced features (e.g. imputation of missing values),
#'  you should consider the \pkg{compositions} or \pkg{robCompositions} package.
#' @references
#'  Aitchison, J. (1986). *The Statistical Analysis of Compositional Data*.
#'  London: Chapman and Hall.
#' @family imputation methods
#' @name missing
#' @rdname missing
NULL

#' Zero-Replacement
#'
#' Multiplicative replacement of zeros.
#' @param x A [`CompositionMatrix-class`] object.
#' @param value A [`numeric`] vector giving the detection limits of each part
#'  (in \eqn{(0,1)}).
#' @param delta A [`numeric`] vector specifying the fraction of the detection
#'  limit to be used in replacement.
#' @return
#'  An [`CompositionMatrix-class`] object, where all zero values have been
#'  replaced.
#' @references
#'  Aitchison, J. (1986). *The Statistical Analysis of Compositional Data*.
#'  London: Chapman and Hall.
#'
#'  Martín-Fernández, J. A., Barceló-Vidal, C. & Pawlowsky-Glahn, V. (2003).
#'  Dealing with Zeros and Missing Values in Compositional Data Sets Using
#'  Nonparametric Imputation. *Mathematical Geology*, 35(3): 253-278.
#'  \doi{10.1023/A:1023866030544}.
#' @example inst/examples/ex-zero.R
#' @author N. Frerebeau
#' @docType methods
#' @family imputation methods
#' @name replace_zero
#' @rdname replace_zero
NULL

#' Missing Values Replacement
#'
#' Multiplicative replacement of missing values.
#' @param x A [`CompositionMatrix-class`] object.
#' @param value A [`numeric`] vector giving the replacement values.
#' @return
#'  An [`CompositionMatrix-class`] object, where all missing values have been
#'  replaced.
#' @references
#'  Martín-Fernández, J. A., Barceló-Vidal, C. & Pawlowsky-Glahn, V. (2003).
#'  Dealing with Zeros and Missing Values in Compositional Data Sets Using
#'  Nonparametric Imputation. *Mathematical Geology*, 35(3): 253-278.
#'  \doi{10.1023/A:1023866030544}.
#' @example inst/examples/ex-missing.R
#' @author N. Frerebeau
#' @docType methods
#' @family imputation methods
#' @name replace_NA
#' @rdname replace_NA
NULL

# Outliers =====================================================================
#' Outlier Detection
#'
#' @param object A [`CompositionMatrix-class`].
#' @inheritParams mahalanobis
#' @param quantile A length-one [`numeric`] vector giving the significance level.
#'  `quantile` is used as a cut-off value for outlier detection: observations
#'  with larger (squared) Mahalanobis distance are considered as potential
#'  outliers.
#' @param groups A `vector` of grouping elements, as long as the variables in
#'  `object`. If a single `character` string is passed, it must be the name of a
#'  categorical variable from the original dataset.
#' @details
#'  An outlier can be defined as having a very large Mahalanobis distance from
#'  all observations. In this way, a certain proportion of the observations can
#'  be identified, e.g. the top 2% of values (i.e. values above the 0.98th
#'  percentile of the Chi-2 distribution).
#'
#'  On the one hand, the Mahalanobis distance is likely to be strongly
#'  affected by the presence of outliers. Rousseeuw and van Zomeren (1990) thus
#'  recommend using robust methods (which are not excessively affected by the
#'  presence of outliers).
#'
#'  On the other hand, the choice of the threshold for classifying an
#'  observation as an outlier should be discussed. There is no apparent reason
#'  why a particular threshold should be applicable to all data sets
#'  (Filzmoser, Garrett, and Reimann 2005).
#' @return
#'  An [`OutlierIndex-class`] object.
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
#'  Rousseeuw, P. J. & van Zomeren, B. C. (1990). Unmasking Multivariate Outliers
#'  and Leverage Points. *Journal of the American Statistical Association*,
#'  85(411): 633-639. \doi{10.1080/01621459.1990.10474920}.
#'
#'  Santos, F. (2020). Modern methods for old data: An overview of some robust
#'  methods for outliers detection with applications in osteology. *Journal of
#'  Archaeological Science: Reports*, 32, 102423.
#'  \doi{10.1016/j.jasrep.2020.102423}.
#' @example inst/examples/ex-outliers.R
#' @author N. Frerebeau
#' @docType methods
#' @family outlier detection methods
#' @aliases outliers-method
setGeneric(
  name = "outliers",
  def = function(object, ...) standardGeneric("outliers")
)

#' Plot Outliers
#'
#' @param x An [`OutlierIndex-class`] object.
#' @param select A length-one vector giving the group to be plotted.
#' @param type A [`character`] string specifying the type of plot that should be
#'  made. It must be one of "`dotchart`", "`distance`" or "`qqplot`".
#'  Any unambiguous substring can be given.
#' @param robust A [`logical`] scalar: should robust Mahalanobis distances be
#'  displayed? Only used if `type` is "`dotchart`" or "`qqplot`".
#' @param pch A lenth-three vector of symbol specification for non-outliers and
#'  outliers (resp.).
#' @param xlim A length-two [`numeric`] vector giving the x limits of the plot.
#'  The default value, `NULL`, indicates that the range of the
#'  [finite][is.finite()] values to be plotted should be used.
#' @param ylim A length-two [`numeric`] vector giving the y limits of the plot.
#'  The default value, `NULL`, indicates that the range of the
#'  [finite][is.finite()] values to be plotted should be used.
#' @param xlab,ylab A [`character`] vector giving the x and y axis labels.
#' @param main A [`character`] string giving a main title for the plot.
#' @param sub A [`character`] string giving a subtitle for the plot.
#' @param ann A [`logical`] scalar: should the default annotation (title and x
#'  and y axis labels) appear on the plot?
#' @param axes A [`logical`] scalar: should axes be drawn on the plot?
#' @param panel.first An an `expression` to be evaluated after the plot axes are
#'  set up but before any plotting takes place. This can be useful for drawing
#'  background grids.
#' @param frame.plot A [`logical`] scalar: should a box be drawn around the
#'  plot?
#' @param panel.last An `expression` to be evaluated after plotting has taken
#'  place but before the axes, title and box are added.
#' @param legend A [`list`] of additional arguments to be passed to
#'  [graphics::legend()]; names of the list are used as argument names.
#'  If `NULL`, no legend is displayed.
#' @param ... Further [graphical parameters][graphics::par()].
#' @return
#'  `plot()` is called for its side-effects: is results in a graphic being
#'  displayed (invisibly return `x`).
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
#' @example inst/examples/ex-outliers.R
#' @author N. Frerebeau
#' @docType methods
#' @family outlier detection methods
#' @name plot_outliers
#' @rdname plot_outliers
NULL

# Sourcing =====================================================================
#' Mixed-Mode Analysis
#'
#' Mixes chemical and petrographic matrices.
#' @param x A [`matrix`] of chemical compositional data or a
#'  [dissimilarity matrix][stats::dist] for these chemical compositional data.
#' @param y A [`matrix`] of coded mineralogical binary data or a
#'  [dissimilarity matrix][stats::dist] for these mineralogical data.
#' @param lambda A length-one [`numeric`] vector giving a weighting factor.
#' @param mu A length-one [`numeric`] vector that lies between 0 and 1 giving
#'  the mixing parameter.
#' @param ... Extra parameters to be passed to [cluster::daisy()].
#' @return
#'  A [stats::dist] object.
#' @references
#'  Baxter, M. J., Beardah, C. C., Papageorgiou, I., Cau, M. A., Day, P. M. &
#'  Kilikoglou, V. (2008). On Statistical Approaches to the Study of Ceramic
#'  Artefacts Using Geochemical and Petrographic Data. *Archaeometry*, 50(1):
#'  142-157. \doi{10.1111/j.1475-4754.2007.00359.x}.
#'
#'  Beardah, C. C., Baxter, M. J., Papageorgiou, I. & Cau, M. A. (2003).
#'  "Mixed-Mode" Approaches to the Grouping of Ceramic Artefacts Using S-Plus.
#'  In M. Doerr and A. Sarris, *The Digital Heritage of Archaeology*, p. 261-266.
#'  Athens: Archive of Monuments and Publications, Hellenic Ministry of Culture.
#'
#'  Gower, J. C. (1971). A general coefficient of similarity and some of its
#'  properties. *Biometrics*, 27(4):857-874. \doi{10.2307/2528823}.
#' @note
#'  **Experimental.**
#' @example inst/examples/ex-mix.R
#' @author N. Frerebeau
#' @docType methods
#' @family sourcing methods
#' @aliases mix-method
setGeneric(
  name = "mix",
  def = function(x, y, ...) standardGeneric("mix"),
  valueClass = "dist"
)
