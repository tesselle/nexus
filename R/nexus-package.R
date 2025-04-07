#' @details
#'  \tabular{ll}{
#'   **Package:** \tab nexus \cr
#'   **Type:** \tab Package \cr
#'   **Version:** \tab 0.5.0 \cr
#'   **License:** \tab GPL-3 \cr
#'   **Zenodo:** \tab \doi{10.5281/zenodo.10225630} \cr
#'  }
#'
#' @section Package options:
#'  \pkg{nexus} uses the following [options()] to configure behavior:
#'  * `nexus.verbose`: a [`logical`] scalar. Should \R report extra information
#'    on progress? Defaults to [interactive()].
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
#' @importFrom isopleuros ternary_pairs
#' @importFrom methods as as<- cbind2 callGeneric callNextMethod
#'  .hasSlot initialize is new rbind2 setClass setGeneric setMethod slot slot<-
#'  slotNames validObject .valueClassTest
#' @importFrom MASS cov.rob
#' @importFrom stats as.dist contr.helmert cov dist hclust mahalanobis ppoints
#'  qchisq qqline qqplot var
#' @importFrom utils combn tail
NULL
