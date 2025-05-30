% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/outliers.R
\docType{methods}
\name{detect_outlier}
\alias{detect_outlier}
\alias{detect_outlier-method}
\alias{is_outlier}
\alias{is_outlier-method}
\alias{detect_outlier,CompositionMatrix,missing-method}
\alias{detect_outlier,CompositionMatrix,CompositionMatrix-method}
\alias{is_outlier,OutlierIndex-method}
\title{Outlier Detection}
\usage{
detect_outlier(object, reference, ...)

is_outlier(object, ...)

\S4method{detect_outlier}{CompositionMatrix,missing}(
  object,
  ...,
  robust = TRUE,
  method = c("mve", "mcd"),
  quantile = 0.975
)

\S4method{detect_outlier}{CompositionMatrix,CompositionMatrix}(
  object,
  reference,
  ...,
  quantile = 0.975,
  robust = TRUE,
  method = c("mve", "mcd")
)

\S4method{is_outlier}{OutlierIndex}(object, robust = TRUE)
}
\arguments{
\item{object}{A \code{\linkS4class{CompositionMatrix}}.}

\item{reference}{A \code{\linkS4class{CompositionMatrix}}. If missing, \code{object} is used.}

\item{...}{Further parameters to be passed to \code{\link[MASS:cov.rob]{MASS::cov.rob()}}.}

\item{robust}{A \code{\link{logical}} scalar: should robust estimators be used?}

\item{method}{A \code{\link{character}} string specifying the method to be used.
It must be one of "\code{mve}" (minimum volume ellipsoid) or "\code{mcd}" (minimum
covariance determinant; see \code{\link[MASS:cov.rob]{MASS::cov.rob()}}).
Only used if \code{robust} is \code{TRUE}.}

\item{quantile}{A length-one \code{\link{numeric}} vector giving the significance level.
\code{quantile} is used as a cut-off value for outlier detection: observations
with larger (squared) Mahalanobis distance are considered as potential
outliers.}
}
\value{
\itemize{
\item \code{detect_outlier()} returns an \code{\linkS4class{OutlierIndex}} object.
\item \code{is_outlier()} returns a \code{\link{logical}} vector.
}
}
\description{
Outlier Detection
}
\details{
An outlier can be defined as having a very large Mahalanobis distance from
all observations. In this way, a certain proportion of the observations can
be identified, e.g. the top 2\% of values (i.e. values above the 0.98th
percentile of the Chi-2 distribution).

On the one hand, the Mahalanobis distance is likely to be strongly
affected by the presence of outliers. Rousseeuw and van Zomeren (1990) thus
recommend using robust methods (which are not excessively affected by the
presence of outliers).

On the other hand, the choice of the threshold for classifying an
observation as an outlier should be discussed. There is no apparent reason
why a particular threshold should be applicable to all data sets
(Filzmoser, Garrett, and Reimann 2005).
}
\examples{
## Data from Day et al. 2011
data("kommos", package = "folio") # Coerce to compositional data
kommos <- remove_NA(kommos, margin = 1) # Remove cases with missing values
coda <- as_composition(kommos, parts = 3:17, groups = 1)

## Detect outliers
out <- detect_outlier(coda)

plot(out, type = "dotchart")
plot(out, type = "distance")

## Detect outliers according to CJ
ref <- group_subset(coda, which = "CJ")
out <- detect_outlier(coda, reference = ref, method = "mcd")
plot(out, type = "dotchart")
}
\references{
Filzmoser, P., Garrett, R. G. & Reimann, C. (2005). Multivariate outlier
detection in exploration geochemistry. \emph{Computers & Geosciences},
31(5), 579-587. \doi{10.1016/j.cageo.2004.11.013}.

Filzmoser, P. & Hron, K. (2008). Outlier Detection for Compositional Data
Using Robust Methods. \emph{Mathematical Geosciences}, 40(3), 233-248.
\doi{10.1007/s11004-007-9141-5}.

Filzmoser, P., Hron, K. & Reimann, C. (2012). Interpretation of multivariate
outliers for compositional data. \emph{Computers & Geosciences}, 39, 77-85.
\doi{10.1016/j.cageo.2011.06.014}.

Rousseeuw, P. J. & van Zomeren, B. C. (1990). Unmasking Multivariate Outliers
and Leverage Points. \emph{Journal of the American Statistical Association},
85(411): 633-639. \doi{10.1080/01621459.1990.10474920}.

Santos, F. (2020). Modern methods for old data: An overview of some robust
methods for outliers detection with applications in osteology. \emph{Journal of
Archaeological Science: Reports}, 32, 102423.
\doi{10.1016/j.jasrep.2020.102423}.
}
\seealso{
Other outlier detection methods: 
\code{\link{plot_outlier}}
}
\author{
N. Frerebeau
}
\concept{outlier detection methods}
