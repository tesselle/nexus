# ACCESSORS
#' @include AllGenerics.R
NULL

# Extract ======================================================================
.subscript1 <- function(x, i) {
  x@.Data[i]
}

.subscript2 <- function(x, i, j, drop) {
  ## Rows
  if (missing(i)) i <- seq_len(nrow(x))
  if (is.character(i)) i <- match(i, dimnames(x)[1L])
  totals <- totals(x)[i]
  groups <- group(x)[i]

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

  methods::initialize(x, z, totals = totals, groups = groups)
}

wrong_dimensions <- function(i, j) {
  msg <- sprintf("M[%s%s]: incorrect number of dimensions.", i, j)
  stop(msg, call. = FALSE)
}

## CompositionMatrix -----------------------------------------------------------
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
      #/!\ DROP /!\
      x <- .subscript2(x, i, , drop = FALSE)
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
      # /!\ DROP /!\
      x <- .subscript2(x, , j, drop = FALSE) # x[j=, ], x[, j=]
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
      # /!\ DROP /!\
      x <- .subscript2(x, i, j, drop = FALSE) # x[i=, j=], x[j=, i=]
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

# Transpose ====================================================================
#' @export
#' @rdname t
#' @aliases t,CompositionMatrix-method
setMethod(
  f = "t",
  signature = c(x = "CompositionMatrix"),
  function(x) {
    t(x@.Data)
  }
)

#' @export
#' @rdname t
#' @aliases t,LogRatio-method
setMethod(
  f = "t",
  signature = c(x = "LogRatio"),
  function(x) {
    t(x@.Data)
  }
)
