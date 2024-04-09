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
    z <- methods::callNextMethod()
    z@extra <- get_features(object)

    ## Set row names
    z@rows@names <- get_identifiers(object)

    ## Set groups (if any)
    if (any_assigned(object))
      z@rows@groups <- get_groups(object)

    z
  }
)
