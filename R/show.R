# SHOW
#' @include AllGenerics.R
NULL

setMethod(
  f = "show",
  signature = "CompositionMatrix",
  definition = function(object) {
    m <- nrow(object)
    p <- ncol(object)
    mtx <- methods::as(object, "matrix")

    txt_dim <- sprintf("%d x %d", m, p)
    txt_mtx <- sprintf("<%s: %s>", class(object), txt_dim)

    cat(
      txt_mtx,
      utils::capture.output(mtx),
      sep = "\n"
    )
    invisible(object)
  }
)
