# ACCESSORS
#' @include AllGenerics.R
NULL

# Extract ======================================================================
## CompositionMatrix -----------------------------------------------------------
.subscript1 <- function(x, i) {
  x@.Data[i]
}

.subscript2 <- function(x, i, j, drop) {
  ## Rows
  if (missing(i)) i <- seq_len(nrow(x))
  if (is.character(i)) i <- match(i, dimnames(x)[1L])
  samples <- x@samples[i]
  groups <- x@groups[i]
  totals <- x@totals[i]

  ## Columns
  if (missing(j)) j <- seq_len(ncol(x))

  ## Subset
  z <- x@.Data[i, j, drop = drop]
  if (drop) return(z)

  ## /!\ Subcomposition /!\
  # if (ncol(z) < ncol(x)) {
  #   tot <- rowSums(z, na.rm = TRUE)
  #   totals <- totals * tot
  #   z <- z / tot
  # }

  methods::initialize(x, z, samples = samples, groups = groups, totals = totals)
}

wrong_dimensions <- function(i, j) {
  msg <- sprintf("M[%s%s]: incorrect number of dimensions.", i, j)
  stop(msg, call. = FALSE)
}

#' @export
#' @rdname subset
#' @aliases [,CompositionMatrix,missing,missing,missing-method
setMethod(
  f = "[",
  signature = c(x = "CompositionMatrix", i = "missing", j = "missing", drop = "missing"),
  definition = function(x, i, j, ..., drop) {
    na <- nargs()
    if (na == 2L) return(x) # x[]
    if (na == 3L) return(x) # x[, ]
    wrong_dimensions(".", ".") # x[, , ], etc.
  }
)

#' @export
#' @rdname subset
#' @aliases [,CompositionMatrix,missing,missing,logical-method
setMethod(
  f = "[",
  signature = c(x = "CompositionMatrix", i = "missing", j = "missing", drop = "logical"),
  definition = function(x, i, j, ..., drop) {
    na <- nargs()
    if (na < 4L) return(x) # x[drop=], x[, drop=], x[drop=, ]
    if (na == 4L) {
      x <- if (drop) x@.Data else x
      return(x) # x[, , drop=], x[, drop=, ], x[drop=, , ]
    }
    wrong_dimensions(".", ".") # x[, , , drop=], etc.
  }
)

#' @export
#' @rdname subset
#' @aliases [,CompositionMatrix,index,missing,missing-method
setMethod(
  f = "[",
  signature = c(x = "CompositionMatrix", i = "index", j = "missing", drop = "missing"),
  definition = function(x, i, j, ..., drop) {
    na <- nargs()
    if (na == 2L) {
      x <- .subscript1(x, i)
      return(x) # x[i=]
    }
    if (na == 3L) {
      x <- .subscript2(x, i, , drop = TRUE)
      return(x) # x[i=, ], x[, i=]
    }
    wrong_dimensions("i", ".") # x[i=, , ], etc.
  }
)

#' @export
#' @rdname subset
#' @aliases [,CompositionMatrix,index,missing,logical-method
setMethod(
  f = "[",
  signature = c(x = "CompositionMatrix", i = "index", j = "missing", drop = "logical"),
  definition = function(x, i, j, ..., drop) {
    na <- nargs()
    if (na == 3L) {
      x <- .subscript1(x, i)
      return(x) # x[i=, drop=]
    }
    if (na == 4L) {
      x <- .subscript2(x, i, , drop = drop)
      return(x) # x[i=, , drop=], x[, i=, drop=]
    }
    wrong_dimensions("i", ".") # x[i=, , , drop=], etc.
  }
)

#' @export
#' @rdname subset
#' @aliases [,CompositionMatrix,missing,index,missing-method
setMethod(
  f = "[",
  signature = c(x = "CompositionMatrix", i = "missing", j = "index", drop = "missing"),
  definition = function(x, i, j, ..., drop) {
    na <- nargs()
    if (na == 2L) {
      x <- .subscript1(x, j) # x[j=]
      return(x)
    }
    if (na == 3L) {
      x <- .subscript2(x, , j, drop = TRUE) # x[j=, ], x[, j=]
      return(x)
    }
    wrong_dimensions(".", "j") # x[, j=, ], etc.
  }
)

#' @export
#' @rdname subset
#' @aliases [,CompositionMatrix,missing,index,logical-method
setMethod(
  f = "[",
  signature = c(x = "CompositionMatrix", i = "missing", j = "index", drop = "logical"),
  definition = function(x, i, j, ..., drop) {
    na <- nargs()
    if (na == 3L) {
      x <- .subscript1(x, j) # x[j=, drop=]
      return(x)
    }
    if (na == 4L) {
      x <- .subscript2(x, , j, drop = drop) # x[j=, , drop=], x[, j=, drop=]
      return(x)
    }
    wrong_dimensions(".", "j") # x[, j=, , drop=], etc.
  }
)

#' @export
#' @rdname subset
#' @aliases [,CompositionMatrix,index,index,missing-method
setMethod(
  f = "[",
  signature = c(x = "CompositionMatrix", i = "index", j = "index", drop = "missing"),
  definition = function(x, i, j, ..., drop) {
    na <- nargs()
    if (na == 3L) {
      x <- .subscript2(x, i, j, drop = TRUE) # x[i=, j=], x[j=, i=]
      return(x)
    }
    wrong_dimensions("i", "j") # x[i=, j=, ], etc.
  }
)

#' @export
#' @rdname subset
#' @aliases [,CompositionMatrix,index,index,logical-method
setMethod(
  f = "[",
  signature = c(x = "CompositionMatrix", i = "index", j = "index", drop = "logical"),
  definition = function(x, i, j, ..., drop) {
    na <- nargs()
    if (na == 4L) {
      x <- .subscript2(x, i, j, drop = drop) # x[i=, j=, drop=], x[j=, i=, drop=]
      return(x)
    }
    wrong_dimensions("i", "j") # x[i=, j=, , drop=], etc.
  }
)

## OutlierIndex ----------------------------------------------------------------
#' @export
#' @rdname subset
#' @aliases [[,OutlierIndex-method
setMethod(
  f = "[[",
  signature = c(x = "OutlierIndex", i = "index"),
  function(x, i) {
    arkhe::assert_length(i, 1)

    mtx <- x[, i, drop = FALSE]
    d <- x@distances[, i, drop = FALSE]

    initialize(x, mtx, distances = d)
  }
)

# Replace ======================================================================
## [<- -------------------------------------------------------------------------
#' @export
#' @rdname subset
#' @aliases [<-,CompositionMatrix-method
setMethod(
  f = "[<-",
  signature = c(x = "CompositionMatrix"),
  function(x, i, j, ..., value) {
    z <- methods::callNextMethod()
    methods::validObject(z)
    z
  }
)

## [[<- ------------------------------------------------------------------------
#' @export
#' @rdname subset
#' @aliases [[<-,CompositionMatrix-method
setMethod(
  f = "[[<-",
  signature = c(x = "CompositionMatrix"),
  function(x, i, j, ..., value) {
    z <- methods::callNextMethod()
    methods::validObject(z)
    z
  }
)
