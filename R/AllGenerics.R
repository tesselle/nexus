# GENERIC METHODS
#' @include AllClasses.R
NULL

# S4 dispatch to base S3 generic ===============================================
setGeneric("dist", package = "stats")
setGeneric("var", package = "stats")
setGeneric("cov", package = "stats")
setGeneric("mahalanobis", package = "stats")
setGeneric("autoplot", package = "ggplot2")

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
#' @param ... Currently not used.
#' @details
#'  The [CompositionMatrix-class] class has special slots
#'  (see `vignette("manual")`):
#'
#'  * `samples` for repeated measurements/observation,
#'  * `groups` to group data by site/area.
#'
#'  When coercing a `data.frame` to a [CompositionMatrix-class] object, an
#'  attempt is made to automatically assign values to these slots by mapping
#'  column names (case insensitive, plural insensitive). This behavior can be
#'  disabled by setting `options(nexus.autodetect = FALSE)` or overridden by
#'  explicitly specifying the columns to be used.
#' @note
#'  All non-numeric variable will be removed.
#' @return A [CompositionMatrix-class] object.
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
#' @param from A [CompositionMatrix-class] object.
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

#' Closure Operation
#'
#' Closes compositions to sum up to 1.
#' @param object A [`numeric`] vector or matrix.
#' @param total A [numeric] vector specifying the total amount to which the
#'  compositions should be closed (defaults to 1).
#' @param ... Currently not used.
#' @section Missing Values Policy:
#'  Missing values will be omitted from the calculations.
#' @return A [`numeric`] vector or matrix (same as `object`).
#' @example inst/examples/ex-coerce.R
#' @author N. Frerebeau
#' @docType methods
#' @family compositional data tools
#' @aliases closure-method
setGeneric(
  name = "closure",
  def = function(object, ...) standardGeneric("closure")
)

# Extract ======================================================================
## Mutators --------------------------------------------------------------------
#' Deal With Groups
#'
#' Retrieves or defines the groups to which the observations belong.
#' @param x An object from which to get or set `groups`.
#' @param value A possible value for the `groups` of `x`.
#' @details
#'  See `vignette("manual")`.
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

#' Deal With Samples
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
#'  See `vignette("manual")`.
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
#' @param object A [CompositionMatrix-class] object.
#' @param ... Currently not used.
#' @return
#'  A [LR-class] object.
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
#' @param object A [CompositionMatrix-class] object.
#' @param weights A [`logical`] scalar: sould a varying weight be used. If
#'  `FALSE` (the default), equally-weighted parts are used. Alternatively, a
#'  positive [`numeric`] vector of weights can be specified.
#' @param ... Currently not used.
#' @return
#'  A [CLR-class] object.
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
#' @param object A [CompositionMatrix-class] object.
#' @param j An [`integer`] giving the index of the rationing part (denominator).
#' @param ... Currently not used.
#' @return
#'  An [ALR-class] object.
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
#' @param object A [CompositionMatrix-class] object.
#' @param ... Currently not used.
#' @details
#'  The ILR transformation provides the coordinates of any composition with
#'  respect to a given orthonormal basis. `transform_ilr()` uses the orthonormal
#'  basis (Helmert matrix) originally defined by Egozcue *et al.* (2003).
#' @return
#'  An [ILR-class] object.
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
#' @param object A [CompositionMatrix-class] object.
#' @param pivot An [`integer`] giving the index of the pivotal variable.
#' @param ... Currently not used.
#' @return
#'  A [PLR-class] object.
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
#' @param object A [CompositionMatrix-class] object.
#' @param ... Currently not used.
#' @return
#'  A [CompositionMatrix-class] object.
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
#' @param x A [CompositionMatrix-class] object.
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
#' @param x A [CompositionMatrix-class] object.
#' @param ... Further argument to be passed to other methods.
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

#' Variance and Covariance
#'
#' @description
#'  * `var()` computes the log-ratio variance matrix.
#'  * `cov()` computes the log-ratio covariance matrix.
#' @param x A [CompositionMatrix-class] object.
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
#' @name covariance
#' @rdname covariance
NULL

#' Variation Matrix
#'
#' Computes the compositional variation array.
#' @param object A [CompositionMatrix-class] object.
#' @param ... Currently not used.
#' @return A [`matrix`].
#' @details
#'  The compositional variation array is a square matrix where the upper
#'  triangular part displays the log-ratio variances and the lower triangular
#'  part displays the log-ratio means.
#' @references
#'  Aitchison, J. (1986). *The Statistical Analysis of Compositional Data*.
#'  London: Chapman and Hall, p. 64-91. \doi{10.1007/978-94-009-4109-0}.
#' @example inst/examples/ex-covariance.R
#' @author N. Frerebeau
#' @docType methods
#' @family statistics
#' @aliases variation-method
setGeneric(
  name = "variation",
  def = function(object, ...) standardGeneric("variation"),
  valueClass = "matrix"
)

# Distances ====================================================================
#' Distances
#'
#' Computes the log-ratio variance matrix.
#' @param x A [CompositionMatrix-class] object.
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
#' @param x A [CompositionMatrix-class] object.
#' @param center A [`numeric`] vector giving the mean vector of the
#'  distribution. If missing, will be estimated from `x`.
#' @param cov A [`numeric`] matrix giving the covariance of the
#'  distribution. If missing, will be estimated from `x`.
#' @param robust A [`logical`] scalar: should robust location and scatter
#'  estimation be used (see [robustbase::covMcd()])?
#' @param ... Extra parameters to be passed to [robustbase::covMcd()].
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
#' @param x,object A [CompositionMatrix-class] object.
#' @param order An [`integer`] vector giving the index of the column to be used
#'  for the ordering of the data.
#' @param decreasing A [`logical`] scalar: should the sort order be increasing
#'  or decreasing?
#' @param facet A [`logical`] scalar: should a matrix of panels defined by
#'  groups be drawn? Only used if `x` [has groups defined][has_groups].
#' @param ... Currently not used.
#' @return
#'  * `autoplot()` returns a [`ggplot`][ggplot2::ggplot] object.
#'  * `plot()` is called it for its side-effects: it results in a graphic being
#'    displayed (invisibly returns `x`).
#' @example inst/examples/ex-plot.R
#' @author N. Frerebeau
#' @docType methods
#' @family plot methods
#' @name plot_coda
#' @rdname plot_coda
NULL

#' Plot Log-Ratios
#'
#' Displays a compositional bar chart.
#' @param x,object A [CompositionMatrix-class] object.
#' @param order A [`logical`] scalar: should the ratio be ordered?
#' @param decreasing A [`logical`] scalar: should the sort order be increasing
#'  or decreasing?
#' @param facet A [`logical`] scalar: should a matrix of panels defined by
#'  groups be drawn? Only used if `x` [has groups defined][has_groups].
#' @param ... Currently not used.
#' @return
#'  * `autoplot()` returns a [`ggplot`][ggplot2::ggplot] object.
#'  * `plot()` is called it for its side-effects: it results in a graphic being
#'    displayed (invisibly returns `x`).
#' @example inst/examples/ex-plot.R
#' @author N. Frerebeau
#' @docType methods
#' @family plot methods
#' @name plot_logratio
#' @rdname plot_logratio
NULL

#' Graph of Log-ratios
#'
#' Produces a graph of log-ratios.
#' @param object A [LogRatio-class] object.
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
#' @param object A [LogRatio-class] object.
#' @inheritParams dimensio::pca
#' @return
#'  A [dimensio::PCA-class] object.
#' @example inst/examples/ex-pca.R
#' @seealso [dimensio::pca()], [dimensio::biplot()]
#' @author N. Frerebeau
#' @docType methods
#' @family multivariate analysis
#' @name pca
#' @rdname pca
NULL

# Outliers =====================================================================
#' Outlier Detection
#'
#' @param object A [CompositionMatrix-class].
#' @inheritParams mahalanobis
#' @param level A length-one [`numeric`] vector giving the significance level.
#'  `level` is used as a cut-off value for outlier detection: observations with
#'  larger (squared) Mahalanobis distance are considered as potential outliers.
#' @param alpha A length-one [`numeric`] vector controlling the size of the
#'  subsets over which the determinant is minimized (see
#'  [robustbase::covMcd()]). Only used if `robust` is `TRUE`.
#' @param ... Currently not used.
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
#'  An [OutlierIndex-class] object.
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
#' @param x,object An [OutlierIndex-class] object.
#' @param qq A [`logical`] scalar: should a quantile-quantile plot be produced?
#' @param limit A [`logical`] scalar: should the cut-off value for outlier
#'  detection be displayed?
#' @param ... Currently not used.
#' @return
#'  * `autoplot()` returns a [`ggplot`][ggplot2::ggplot] object.
#'  * `plot()` and `qqplot()` are called it for their side-effects: they result
#'    in a graphic being displayed (invisibly return `x`).
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
