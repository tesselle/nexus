#' @details
#'  \tabular{ll}{
#'   **Package:** \tab nexus \cr
#'   **Type:** \tab Package \cr
#'   **Version:** \tab 0.0.0.9000 \cr
#'   **License:** \tab GPL-3 \cr
#'   **Zenodo:** \tab  \cr
#'  }
#'
#' @section Package options:
#'  `nexus` uses the following [options()] to configure behavior:
#'  * `nexus.autodetect`: a [`logical`] scalar. Try to automatically assign
#'    values to the corresponding slot of a `CompositionMatrix` object when
#'    coercing a `data.frame`? Defaults to `TRUE`.
#'  * `nexus.verbose`: a [`logical`] scalar. Should \R report extra information
#'    on progress? Defaults to `TRUE`.
#'
#' @author
#'  **Full list of authors and contributors** (alphabetic order)
#'
#'  \tabular{ll}{
#'   Nicolas Frerebeau \tab *Université Bordeaux Montaigne, France* \cr
#'   Brice Lebrun \tab *Université Bordeaux Montaigne, France* \cr
#'   Anne Philippe \tab *Université de Nantes, France* \cr
#'  }
#'
#'  **Package maintainer**
#'
#' Nicolas Frerebeau\cr
#' \email{nicolas.frerebeau@@u-bordeaux-montaigne.fr}
#'
#' Archéosciences Bordeaux (UMR 6034)\cr
#' Maison de l'Archéologie\cr
#' Université Bordeaux Montaigne\cr
#' F-33607 Pessac cedex\cr
#' France
#' @name nexus-package
#' @aliases nexus-package nexus
#' @docType package
#' @keywords internal
"_PACKAGE"

#' @import arkhe
#' @importMethodsFrom dimensio pca
#' @importFrom methods as as<- callGeneric callNextMethod
#'  .hasSlot initialize is new setClass setGeneric setMethod slot slot<-
#'  slotNames validObject .valueClassTest
#' @importFrom MASS cov.rob
#' @importFrom stats as.dist contr.helmert cov dist hclust mahalanobis ppoints
#'  qchisq var
#' @importFrom utils combn tail
NULL
