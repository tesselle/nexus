# MUTATORS
#' @include AllGenerics.R
NULL

# Not exported
get_transformation <- function(x) {
  if (methods::is(x, "LR")) return("Pairwise Log-Ratio")
  if (methods::is(x, "CLR")) return("Centered Log-Ratio")
  if (methods::is(x, "ALR")) return("Additive Log-Ratio")
  if (methods::is(x, "ILR")) return("Isometric Log-Ratio")
  if (methods::is(x, "PLR")) return("Pivot Log-Ratio")
}

# Predicates ===================================================================
#' @export
#' @rdname CompositionMatrix-class
is_composition <- function(object) {
  methods::is(object, "CompositionMatrix")
}

#' @export
#' @rdname LogRatio-class
is_logratio <- function(object) {
  methods::is(object, "LogRatio")
}

#' @export
#' @rdname ReferenceGroups-class
is_grouped <- function(object) {
  methods::is(object, "ReferenceGroups")
}

# Getter =======================================================================
#' @export
#' @method labels CompositionMatrix
labels.CompositionMatrix <- function(object, ...) {
  colnames(object)
}

#' @export
#' @rdname mutators
#' @aliases labels,CompositionMatrix-method
setMethod("labels", "CompositionMatrix", labels.CompositionMatrix)

#' @export
#' @method labels LogRatio
labels.LogRatio <- function(object, ...) {
  object@ratio
}

#' @export
#' @rdname mutators
#' @aliases labels,LogRatio-method
setMethod("labels", "LogRatio", labels.LogRatio)

# Weights ======================================================================
#' @export
#' @method weights ALR
weights.ALR <- function(object, ...) {
  w <- object@weights
  w[-1] * w[1]
}

#' @export
#' @rdname mutators
#' @aliases weights,ALR-method
setMethod("weights", "ALR", weights.ALR)

#' @export
#' @method weights LR
weights.LR <- function(object, ...) {
  w <- object@weights
  w <- utils::combn(
    x = w,
    m = 2,
    FUN = function(x) Reduce(`*`, x),
    simplify = FALSE
  )
  unlist(w)
}

#' @export
#' @rdname mutators
#' @aliases weights,LR-method
setMethod("weights", "LR", weights.LR)

#' @export
#' @method weights LogRatio
weights.LogRatio <- function(object, ...) {
  object@weights
}

#' @export
#' @rdname mutators
#' @aliases weights,LogRatio-method
setMethod("weights", "LogRatio", weights.LogRatio)

# Totals =======================================================================
#' @export
#' @rdname totals
#' @aliases totals,CompositionMatrix-method
setMethod("totals", "CompositionMatrix", function(object) object@totals)

#' @export
#' @rdname totals
#' @aliases totals,LogRatio-method
setMethod("totals", "LogRatio", function(object) object@totals)

#' @export
#' @rdname totals
setMethod(
  f = "totals<-",
  signature = "CompositionMatrix",
  definition = function(object, value) {
    object@totals <- if (is.null(value)) rowSums(object) else as.numeric(value)
    methods::validObject(object)
    object
  }
)
