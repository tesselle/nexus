% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/coerce.R
\docType{methods}
\name{as.data.frame}
\alias{as.data.frame}
\alias{as.data.frame,CompositionMatrix-method}
\alias{as.data.frame,LogRatio-method}
\alias{as.data.frame,OutlierIndex-method}
\title{Coerce to a Data Frame}
\usage{
\S4method{as.data.frame}{CompositionMatrix}(x, ..., group_var = ".group", group_after = 0)

\S4method{as.data.frame}{LogRatio}(x, ..., group_var = ".group", group_after = 0)

\S4method{as.data.frame}{OutlierIndex}(x, row.names = rownames(x), optional = FALSE, ...)
}
\arguments{
\item{x}{An \R object (typically, a \code{\linkS4class{CompositionMatrix}} object).}

\item{...}{Further parameters to be passed to
\code{\link[base:as.data.frame]{as.data.frame()}}.}

\item{group_var}{A \code{\link{character}} string specifying the name of the column to
create for group attribution (only used if \code{x} is \link[=group]{grouped}).}

\item{group_after}{An \code{\link{integer}} specifying a subscript, after which the new
\code{group_var} column is to be appended (only used if \code{x} is \link[=group]{grouped}).}

\item{row.names}{A \code{\link{character}} vector giving the row names for the data
frame, or \code{NULL}.}

\item{optional}{A \code{\link{logical}} scalar: should the names of the variables in
the data frame be checked? If \code{FALSE} then the names of the variables in the
data frame are checked to ensure that they are syntactically valid variable
names and are not duplicated.}
}
\value{
A \code{\link{data.frame}}.
}
\description{
Coerce to a Data Frame
}
\examples{
## Create a count matrix
A1 <- matrix(data = sample(1:100, 100, TRUE), nrow = 20)

## Coerce to compositions
B <- as_composition(A1)

## Row sums are internally stored before coercing to relative frequencies
totals(B)

## This allows to restore the source data
A2 <- as_amounts(B)

## Coerce to a data.frame
X <- as.data.frame(B)
head(X)
}
\seealso{
Other mutators: 
\code{\link{labels}()},
\code{\link{totals}()},
\code{\link{weights}()}
}
\author{
N. Frerebeau
}
\concept{mutators}
