# GENERIC METHODS
#' @include AllClasses.R
NULL

# S4 dispatch to base S3 generic ===============================================
setGeneric("dist", package = "stats")
setGeneric("mahalanobis", package = "stats")

# CoDa =========================================================================
#' Coerce to a Closed Compositional Matrix
#'
#' Coerces an object to a `CompositionMatrix` object.
#' @param from A [`matrix`] or [`data.frame`] to be coerced.
#' @param samples An [`integer`] giving the index of the column to be used for
#'  sample identification: allows to identify replicated measurements.
#'  If `NULL` (the default), row names will be used as sample IDs.
#' @param groups An [`integer`] giving the index of the column to be used to
#'  group the samples. If `NULL` (the default), no grouping is stored.
#' @param verbose A [`logical`] scalar: should \R report extra information
#'  on progress?
#' @param ... Currently not used.
#' @details
#'  The [`CompositionMatrix-class`] class has special slots:
#'
#'  * `samples` for [repeated measurements/observation][samples],
#'  * `groups` to [group data by site/area][group].
#'
#'  When coercing a `data.frame` to a [`CompositionMatrix-class`] object, an
#'  attempt is made to automatically assign values to these slots by mapping
#'  column names (case insensitive, plural insensitive). This behavior can be
#'  disabled by setting `options(nexus.autodetect = FALSE)` or overridden by
#'  explicitly specifying the columns to be used.
#'
#'  See `vignette("nexus")`.
#' @note
#'  All non-numeric variable will be removed.
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

# Simplex ======================================================================
#' Arithmetic Operators
#'
#' Operators performing operations in the simplex.
#' @param e1,e2 A [`CompositionMatrix-class`] object or a [`numeric`] vector or
#'  matrix.
#' @details
#'  \describe{
#'   \item{`+`}{[Perturbation operation][perturbation()]}
#'   \item{`-`}{[Perturbation operation][perturbation()]}
#'   \item{`*`}{[Powering operation][powering()]}
#' }
#' @return
#'  A [`CompositionMatrix-class`] object or a [`numeric`] vector (same as `e1`).
#' @example inst/examples/ex-arith.R
#' @author N. Frerebeau
#' @docType methods
#' @family operations in the simplex
#' @name arithmetic
#' @rdname arithmetic
NULL

#' Closure Operation
#'
#' Closes compositions to sum up to 1.
#' @param x A [`numeric`] vector or matrix.
#' @param total A [numeric] vector specifying the total amount to which the
#'  compositions should be closed (defaults to 1).
#' @param na.rm A [`logical`] scalar: should missing values be removed?
#' @param ... Currently not used.
#' @return A [`numeric`] vector or matrix (same as `x`).
#' @example inst/examples/ex-coerce.R
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
#' @param x,y A [`numeric`] vector of compositional data.
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
#' @param x A [`numeric`] vector of compositional data.
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

# Extract ======================================================================
## Mutators --------------------------------------------------------------------
#' Working With Groups
#'
#' Retrieves or defines the groups to which the observations belong.
#' @param x An object from which to get or set `groups`.
#' @param value A possible value for the `groups` of `x`.
#' @details
#'  See `vignette("nexus")`.
#' @return
#'  * `set_groups()` returns an object of the same sort as `x` with the new
#'    group names assigned.
#'  * `get_groups()` returns the group names of `x`.
#'  * `has_groups()` returns a [`logical`] scalar.
#' @author N. Frerebeau
#' @docType methods
#' @family mutators
#' @name groups
#' @rdname groups
NULL

#' @rdname groups
#' @aliases has_groups-method
setGeneric(
  name = "has_groups",
  def = function(x) standardGeneric("has_groups")
)

#' @rdname groups
#' @aliases get_groups-method
setGeneric(
  name = "get_groups",
  def = function(x) standardGeneric("get_groups")
)

#' @rdname groups
#' @aliases set_groups-method
setGeneric(
  name = "set_groups<-",
  def = function(x, value) standardGeneric("set_groups<-")
)

#' Working With Samples
#'
#' Retrieves or defines the sample names.
#' @param x An object from which to get or set `samples`.
#' @param value A possible value for the `samples` of `x`.
#' @details
#'  In some situations, measurements may have been repeated (e.g. multiple
#'  chemical analyses on the same sample). The presence of repeated
#'  measurements can be specified by giving several observations the same
#'  sample name.
#'
#'  See `vignette("nexus")`.
#' @return
#'  * `set_samples()` returns an object of the same sort as `x` with the new
#'    sample names assigned.
#'  * `get_samples()` returns the sample names of `x`.
#'  * `has_replicates()` returns a [`logical`] scalar.
#' @author N. Frerebeau
#' @docType methods
#' @family mutators
#' @name samples
#' @rdname samples
NULL

#' @rdname samples
#' @aliases has_replicates-method
setGeneric(
  name = "has_replicates",
  def = function(x) standardGeneric("has_replicates")
)

#' @rdname samples
#' @aliases get_samples-method
setGeneric(
  name = "get_samples",
  def = function(x) standardGeneric("get_samples")
)

#' @rdname samples
#' @aliases set_samples-method
setGeneric(
  name = "set_samples<-",
  def = function(x, value) standardGeneric("set_samples<-")
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
#'  `NULL`. Numeric values are coerced to [`integer`] as by [as.integer()]
#'  (and hence truncated towards zero). Character vectors will be matched to
#'  the name of the elements. An empty index (a comma separated blank) indicates
#'  that all entries in that dimension are selected.
#' @param value A possible value for the element(s) of `x`.
#' @param drop A [`logical`] scalar: should the result be coerced to
#'  the lowest possible dimension? This only works for extracting elements,
#'  not for the replacement.
#' @param ... Currently not used.
#' @return
#'  A subsetted object of the same sort as `x`.
#' @example inst/examples/ex-matrix.R
#' @author N. Frerebeau
#' @docType methods
#' @family mutators
#' @name subset
#' @rdname subset
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
#'  London: Chapman and Hall. \doi{10.1007/978-94-009-4109-0}.
#'
#'  Greenacre, M. J. (2019). *Compositional Data Analysis in Practice*.
#'  Boca Raton: CRC Press.
#' @example inst/examples/ex-transform.R
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
#' @return
#'  A [`CLR-class`] object.
#' @references
#'  Aitchison, J. (1986). *The Statistical Analysis of Compositional Data*.
#'  London: Chapman and Hall. \doi{10.1007/978-94-009-4109-0}.
#'
#'  Greenacre, M. J. (2019). *Compositional Data Analysis in Practice*.
#'  Boca Raton: CRC Press.
#' @example inst/examples/ex-transform.R
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
#' @return
#'  An [`ALR-class`] object.
#' @references
#'  Aitchison, J. (1986). *The Statistical Analysis of Compositional Data*.
#'  London: Chapman and Hall. \doi{10.1007/978-94-009-4109-0}.
#'
#'  Greenacre, M. J. (2019). *Compositional Data Analysis in Practice*.
#'  Boca Raton: CRC Press.
#' @example inst/examples/ex-transform.R
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
#' @example inst/examples/ex-transform.R
#' @author N. Frerebeau
#' @docType methods
#' @family log-ratio transformations
#' @aliases transform_ilr-method
setGeneric(
  name = "transform_ilr",
  def = function(object, ...) standardGeneric("transform_ilr"),
  valueClass = "ILR"
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
#'  Hron, K., Filzmoser, P., de Caritat, P., Fišerová, E. & Gardlo, A. (2017).
#'  Weighted Pivot Coordinates for Compositional Data and Their Application to
#'  Geochemical Mapping. *Mathematical Geosciences*, 49(6), 797-814.
#'  \doi{10.1007/s11004-017-9684-z}.
#'
#'  Greenacre, M. J. (2019). *Compositional Data Analysis in Practice*.
#'  Boca Raton: CRC Press.
#' @example inst/examples/ex-transform.R
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
#' @param object A [`CompositionMatrix-class`] object.
#' @param ... Currently not used.
#' @return
#'  A [`CompositionMatrix-class`] object.
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
#'
#'  Greenacre, M. J. (2019). *Compositional Data Analysis in Practice*.
#'  Boca Raton: CRC Press.
#' @example inst/examples/ex-transform.R
#' @author N. Frerebeau
#' @docType methods
#' @family log-ratio transformations
#' @aliases transform_inverse-method
setGeneric(
  name = "transform_inverse",
  def = function(object, ...) standardGeneric("transform_inverse"),
  valueClass = "CompositionMatrix"
)

# Statistics ===================================================================
#' Compute Summary Statistics of Data Subsets
#'
#' Splits the data into subsets, computes summary statistics for each, and
#' returns the result.
#' @param x A [`CompositionMatrix-class`] object.
#' @param by A [`character`] string specifying the grouping element. It must be
#'  one of "`samples`" or "`groups`". Any unambiguous substring can be given.
#' @param FUN A [`function`] to compute the summary statistics.
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
#' @param na.rm A [`logical`] scalar: should missing values be removed?
#' @param ... Currently not used.
#' @details
#'  Closed vector of the columns geometric means.
#' @return A [`numeric`] vector.
#' @references
#'  Aitchison, J. (1986). *The Statistical Analysis of Compositional Data*.
#'  London: Chapman and Hall, p. 64-91. \doi{10.1007/978-94-009-4109-0}.
#' @example inst/examples/ex-mean.R
#' @author N. Frerebeau
#' @docType methods
#' @family statistics
#' @name mean
#' @rdname mean
NULL

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
#'  London: Chapman and Hall, p. 64-91. \doi{10.1007/978-94-009-4109-0}.
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
#'  London: Chapman and Hall, p. 64-91. \doi{10.1007/978-94-009-4109-0}.
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
#  London: Chapman and Hall, p. 64-91. \doi{10.1007/978-94-009-4109-0}.
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
#'  London: Chapman and Hall, p. 64-91. \doi{10.1007/978-94-009-4109-0}.
#'
#'  Greenacre, M. J. (2019). *Compositional Data Analysis in Practice*.
#'  Boca Raton: CRC Press.
#' @example inst/examples/ex-distance.R
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
#' @seealso [stats::mahalanobis()]
#' @author N. Frerebeau
#' @docType methods
#' @family statistics
#' @name mahalanobis
#' @rdname mahalanobis
NULL

# Plot =========================================================================
#' Plot Compositional Data
#'
#' Displays a compositional bar chart.
#' @param x A [`CompositionMatrix-class`] object.
#' @param order An [`integer`] vector giving the index of the column to be used
#'  for the ordering of the data.
#' @param decreasing A [`logical`] scalar: should the sort order be increasing
#'  or decreasing?
#' @param groups A [`factor`] in the sense that [`as.factor(groups)`][as.factor()]
#'  defines the grouping. If set, a matrix of panels defined by `groups` will be
#'  drawn.
#' @param horiz A [`logical`] scalar. If `FALSE`, the bars are drawn vertically
#'  with the first bar to the left. If `TRUE` (the default), the bars are drawn
#'  horizontally with the first at the bottom.
#' @param xlab,ylab A [`character`] vector giving the x and y axis labels.
#' @param main A [`character`] string giving a main title for the plot.
#' @param sub A [`character`] string giving a subtitle for the plot.
#' @param ann A [`logical`] scalar: should the default annotation (title and x
#'  and y axis labels) appear on the plot?
#' @param axes A [`logical`] scalar: should axes be drawn on the plot?
#' @param ... Further parameters to be passed to [graphics::barplot()].
#' @return
#'  `plot()` is called for its side-effects: is results in a graphic being
#'  displayed (invisibly return `x`).
#' @example inst/examples/ex-plot.R
#' @author N. Frerebeau
#' @docType methods
#' @family plot methods
#' @name plot_coda
#' @rdname plot_coda
NULL

#' Plot Log-Ratios
#'
#' Displays a density plot.
#' @param x A [`LogRatio-class`] object.
#' @param order A [`logical`] scalar: should the ratio be ordered?
#' @param decreasing A [`logical`] scalar: should the sort order be increasing
#'  or decreasing?
#' @param groups A [`factor`] in the sense that [`as.factor(groups)`][as.factor()]
#'  defines the grouping. If set, a matrix of panels defined by `groups` will be
#'  drawn.
#' @param rug A [`logical`] scalar: should a *rug* representation (1-d plot) of
#'  the data be added to the plot?
#' @param ticksize A length-one [`numeric`] vector giving the length of the
#'  ticks making up the *rug*. Positive lengths give inwards ticks. Only used if
#'  `rug` is `TRUE`.
#' @param flip A [`logical`] scalar: should the y-axis (ticks and numbering) be
#'  flipped from side 2 (left) to 4 (right) from series to series when `facet`
#'  is "`multiple`"?
#' @param ncol An [`integer`] specifying the number of columns to use when
#'  `facet` is "`multiple`". Defaults to 1 for up to 4 series, otherwise to 2.
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
#' @example inst/examples/ex-plot-logratio.R
#' @author N. Frerebeau
#' @docType methods
#' @family plot methods
#' @name plot_logratio
#' @rdname plot_logratio
NULL

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
#'  A [`dimensio::PCA-class`] object.
#' @example inst/examples/ex-pca.R
#' @seealso [dimensio::pca()], [dimensio::biplot()]
#' @author N. Frerebeau
#' @docType methods
#' @family multivariate analysis
#' @name pca_coda
#' @rdname pca_coda
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
#' @param qq A [`logical`] scalar: should a quantile-quantile plot be produced?
#' @param probs A length-two [`numeric`] vector representing probabilities.
#'  Corresponding quantile pairs define the line drawn (see [stats::qqline()]).
#'  Only used if `qq` is `TRUE`.
#' @param limit A [`logical`] scalar: should the cut-off value for outlier
#'  detection be displayed? Only used if `qq` is `FALSE`.
#' @param col A vector of colors.
#' @param pch A vector of plotting `character` (symbol).
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
