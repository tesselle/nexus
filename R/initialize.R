# INITIALIZE
#' @include AllClasses.R
NULL

#' @export
#' @rdname CompositionMatrix
CompositionMatrix <- function(data = 0, nrow = 1, ncol = 1, byrow = FALSE,
                              dimnames = NULL) {

  if (is.object(data) || !is.atomic(data)) data <- as.vector(data)

  ## Creates a matrix
  mtx <- .Internal(matrix(data, nrow, ncol, byrow, dimnames, missing(nrow),
                          missing(ncol)))

  ## Normalize
  ## TODO: prevent division by zero?
  totals <- rowSums(mtx)
  mtx <- mtx / totals

  ## Make dimnames
  row_names <- make_names(dimnames[[1L]], nrow(mtx), "row")
  column_names <- make_names(dimnames[[2L]], ncol(mtx), "col")
  dimnames(mtx) <- list(row_names, column_names)

  .CompositionMatrix(mtx, totals = totals, samples = rownames(mtx))
}

make_names <- function(x, n = 0, prefix = "var") {
  if (is.null(x)) {
    x <- if (n > 0) paste0(prefix, seq_len(n)) else character(0)
  } else {
    x <- make.unique(as.character(x), sep = "_")
  }
  x
}

make_dimnames <- function(x) {
  list(
    make_names(dimnames(x)[[1L]], nrow(x), "row"),
    make_names(dimnames(x)[[2L]], ncol(x), "col")
  )
}
