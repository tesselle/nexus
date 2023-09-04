% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/coerce.R
\docType{methods}
\name{as_composition}
\alias{as_composition}
\alias{as_composition-method}
\alias{as_composition,matrix-method}
\alias{as_composition,data.frame-method}
\title{Coerce to a Closed Compositional Matrix}
\usage{
as_composition(from, ...)

\S4method{as_composition}{matrix}(from)

\S4method{as_composition}{data.frame}(from, samples = NULL, groups = NULL)
}
\arguments{
\item{from}{A \code{\link{matrix}} or \code{\link{data.frame}} to be coerced.}

\item{...}{Currently not used.}

\item{samples}{An \code{\link{integer}} giving the index of the column to be used for
sample identification: allows to identify replicated measurements.
If \code{NULL} (the default), row names will be used as sample IDs.}

\item{groups}{An \code{\link{integer}} giving the index of the column to be used to
group the samples. If \code{NULL} (the default), no grouping is stored.}
}
\value{
A \linkS4class{CompositionMatrix} object.
}
\description{
Coerces an object to a \code{CompositionMatrix} object.
}
\details{
The \linkS4class{CompositionMatrix} class has special slots
(see \code{vignette("manual")}):
\itemize{
\item \code{samples} for repeated measurements/observation,
\item \code{groups} to group data by site/area.
}

When coercing a \code{data.frame} to a \linkS4class{CompositionMatrix} object, an
attempt is made to automatically assign values to these slots by mapping
column names (case insensitive, plural insensitive). This behavior can be
disabled by setting \code{options(nexus.autodetect = FALSE)} or overridden by
explicitly specifying the columns to be used.
}
\note{
All non-numeric variable will be removed.
}
\examples{
## Create a count matrix
A1 <- matrix(data = as.numeric(sample(1:100, 100, TRUE)), nrow = 20)

## Coerce to compositions
B <- as_composition(A1)

## Row sums are internally stored before coercing to relative frequencies
## (use get_totals() to retrieve these values)
## This allows to restore the source data
A2 <- as_amounts(B)

## Coerce to an S3 data.frame
X <- data.frame(B)
head(X)
}
\seealso{
Other compositional data tools: 
\code{\link{as_amounts}()},
\code{\link{closure}()}
}
\author{
N. Frerebeau
}
\concept{compositional data tools}