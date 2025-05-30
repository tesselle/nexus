% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/subset.R
\docType{methods}
\name{subset}
\alias{subset}
\alias{[,CompositionMatrix,missing,missing,missing-method}
\alias{[,CompositionMatrix,missing,missing,logical-method}
\alias{[,CompositionMatrix,index,missing,missing-method}
\alias{[,CompositionMatrix,index,missing,logical-method}
\alias{[,CompositionMatrix,missing,index,missing-method}
\alias{[,CompositionMatrix,missing,index,logical-method}
\alias{[,CompositionMatrix,index,index,missing-method}
\alias{[,CompositionMatrix,index,index,logical-method}
\alias{[<-,CompositionMatrix-method}
\alias{[[<-,CompositionMatrix-method}
\title{Extract or Replace Parts of an Object}
\usage{
\S4method{[}{CompositionMatrix,missing,missing,missing}(x, i, j, ..., drop = TRUE)

\S4method{[}{CompositionMatrix,missing,missing,logical}(x, i, j, ..., drop = TRUE)

\S4method{[}{CompositionMatrix,index,missing,missing}(x, i, j, ..., drop = TRUE)

\S4method{[}{CompositionMatrix,index,missing,logical}(x, i, j, ..., drop = TRUE)

\S4method{[}{CompositionMatrix,missing,index,missing}(x, i, j, ..., drop = TRUE)

\S4method{[}{CompositionMatrix,missing,index,logical}(x, i, j, ..., drop = TRUE)

\S4method{[}{CompositionMatrix,index,index,missing}(x, i, j, ..., drop = TRUE)

\S4method{[}{CompositionMatrix,index,index,logical}(x, i, j, ..., drop = TRUE)

\S4method{[}{CompositionMatrix}(x, i, j, ...) <- value

\S4method{[[}{CompositionMatrix}(x, i, j, ...) <- value
}
\arguments{
\item{x}{An object from which to extract element(s) or in which to replace
element(s).}

\item{i, j}{Indices specifying elements to extract or replace. Indices are
\code{\link{numeric}}, \code{\link{integer}} or \code{\link{character}} vectors or empty (missing) or
\code{NULL}. Numeric values are coerced to \code{\link{integer}} as by \code{\link[=as.integer]{as.integer()}}.
Character vectors will be matched to the name of the elements.
An empty index (a comma separated blank) indicates that all entries in that
dimension are selected.}

\item{...}{Currently not used.}

\item{drop}{A \code{\link{logical}} scalar: should the result be coerced to
the lowest possible dimension? This only works for extracting elements,
not for the replacement. Defaults to \code{FALSE}.}

\item{value}{A possible value for the element(s) of \code{x}.}
}
\value{
A subsetted object of the same sort as \code{x}.
}
\description{
Operators acting on objects to extract or replace parts.
}
\examples{
## Data from Aitchison 1986
data("hongite")

## Coerce to compositional data
coda <- as_composition(hongite)
head(coda)

## Subset
coda[[1, 1]] # Get the first value
coda[1] # Get the first value
coda[, ] # Get all values
coda[1, ] # Get the first row

## Subcomposition
subcoda <- coda[, 1:3] # Get the first three column
head(subcoda)
}
\seealso{
Other subsetting methods: 
\code{\link{bind}}
}
\author{
N. Frerebeau
}
\concept{subsetting methods}
