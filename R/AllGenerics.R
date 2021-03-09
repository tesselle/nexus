# GENERIC METHODS
#' @include AllClasses.R
NULL

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
