# PCA
#' @include AllGenerics.R
NULL

#' @export
#' @describeIn pca PCA of centered log-ratio, i.e. log-ratio analysis (LRA).
#' @aliases pca,CompositionMatrix-method
setMethod(
  f = "pca",
  signature = c("CompositionMatrix"),
  definition = function(object, center = TRUE, scale = FALSE, rank = NULL,
                        sup_row = NULL, sup_col = NULL,
                        weight_row = NULL, weight_col = NULL) {
    message("PCA of centered log-ratio.")
    x <- transform_clr(object)
    methods::callGeneric(object = x, center = center, scale = scale,
                         rank = rank, sup_row = sup_row, sup_col = sup_col,
                         weight_row = weight_row, weight_col = weight_col)
  }
)

#' @export
#' @rdname pca
#' @aliases pca,LogRatio-method
setMethod(
  f = "pca",
  signature = c("LogRatio"),
  definition = function(object, center = TRUE, scale = FALSE, rank = NULL,
                        sup_row = NULL, sup_col = NULL,
                        weight_row = NULL, weight_col = NULL) {
    x <- methods::callNextMethod()
    if (any_assigned(object)) x@rows@groups <- get_groups(object)
    x
  }
)
