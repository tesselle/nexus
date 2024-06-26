% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/statistics.R
\docType{methods}
\name{metric_var}
\alias{metric_var}
\alias{metric_var-method}
\alias{metric_sd}
\alias{metric_sd-method}
\alias{metric_var,CompositionMatrix-method}
\alias{metric_sd,CompositionMatrix-method}
\title{Metric Variance and Standard Deviation}
\usage{
metric_var(x, ...)

metric_sd(x, ...)

\S4method{metric_var}{CompositionMatrix}(x)

\S4method{metric_sd}{CompositionMatrix}(x)
}
\arguments{
\item{x}{A \code{\linkS4class{CompositionMatrix}} object.}

\item{...}{Currently not used.}
}
\value{
A \code{\link{numeric}} vector.
}
\description{
\itemize{
\item \code{metric_var()} computes the metric variance (or total variance), i.e. a
global measure of spread.
\item \code{metric_sd()} computes the metric standard deviation.
}
}
\details{
The metric variance is the average of the \link[=transform_clr]{CLR} variances.
}
\examples{
## Data from Aitchison 1986
data("hongite")

## Coerce to compositional data
coda <- as_composition(hongite)

## Mean
mean(coda)

## Quantile
quantile(coda)

## Metric variance
metric_var(coda)

## Metric standard deviation
metric_sd(coda)
}
\references{
Boogaart, K. G. van den & Tolosana-Delgado, R. (2013). \emph{Analyzing
Compositional Data with R}. Berlin Heidelberg: Springer-Verlag.
\doi{10.1007/978-3-642-36809-7}.

Hron, K. & Kubáček. L. (2011). Statistical Properties of the Total Variation
Estimator for Compositional Data. \emph{Metrika}, 74 (2): 221-230.
\doi{10.1007/s00184-010-0299-3}.

Pawlowsky-Glahn, V. & Egozcue, J. J. (2001). Geometric Approach to
Statistical Analysis on the Simplex. \emph{Stochastic Environmental Research and
Risk Assessment}, 15(5): 384-398. \doi{10.1007/s004770100077}.
}
\seealso{
Other statistics: 
\code{\link{aggregate}()},
\code{\link{condense}()},
\code{\link{covariance}()},
\code{\link{dist}},
\code{\link{mahalanobis}()},
\code{\link{margin}()},
\code{\link{mean}()},
\code{\link{pip}()},
\code{\link{quantile}()},
\code{\link{scale}()},
\code{\link{variation}()}
}
\author{
N. Frerebeau
}
\concept{statistics}
