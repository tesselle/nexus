# PCA
#' @include AllGenerics.R
NULL

#' @export
#' @rdname pca
#' @aliases pca,CompositionMatrix-method
setMethod(
  f = "pca",
  signature = c("CompositionMatrix"),
  definition = function(object, center = TRUE, scale = FALSE, rank = NULL,
                        sup_row = NULL, sup_col = NULL,
                        weight_row = NULL, weight_col = NULL) {
    stop("You should not do that! Transform your data first.", call. = FALSE)
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
    methods::callNextMethod()
  }
)
