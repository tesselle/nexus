# PCA
#' @include AllClasses.R AllGenerics.R
NULL

#' @importFrom dimensio pca
#' @export
#' @rdname pca
#' @aliases pca,CompositionMatrix-method
setMethod(
  f = "pca",
  signature = signature(object = "CompositionMatrix"),
  definition = function(object, center = TRUE, scale = TRUE, rank = NULL,
                        sup_row = NULL, sup_col = NULL,
                        weight_row = NULL, weight_col = NULL) {
    stop("You shouldn't do that! Transform your data first.", call. = FALSE)
  }
)

#' @importFrom dimensio pca
#' @export
#' @rdname pca
#' @aliases pca,LogRatio-method
setMethod(
  f = "pca",
  signature = signature(object = "LogRatio"),
  definition = function(object, center = TRUE, scale = TRUE, rank = NULL,
                        sup_row = NULL, sup_col = NULL,
                        weight_row = NULL, weight_col = NULL) {
    methods::callNextMethod()
  }
)
