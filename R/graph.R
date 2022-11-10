# GRAPH
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname graph
#' @aliases graph,LR-method
setMethod(
  f = "graph",
  signature = signature(object = "LR"),
  definition = function(object) {
    ## Validation
    needs("igraph")

    ratio <- object@ratio
    edges <- do.call(rbind, strsplit(ratio, "_"))
    edges <- edges[, c(2, 1)]
    igraph::graph_from_edgelist(edges, directed = FALSE)
  }
)

#' @export
#' @rdname graph
#' @aliases graph,ALR-method
setMethod(
  f = "graph",
  signature = signature(object = "ALR"),
  definition = function(object) {
    ## Validation
    needs("igraph")

    ratio <- object@ratio
    edges <- do.call(rbind, strsplit(ratio, "_"))
    edges <- edges[, c(2, 1)]
    igraph::graph_from_edgelist(edges, directed = TRUE)
  }
)

#' @export
#' @rdname graph
#' @aliases graph,ILR-method
setMethod(
  f = "graph",
  signature = signature(object = "ILR"),
  definition = function(object) {
    ## Validation
    needs("igraph")

    ratio <- object@ratio
    edges <- lapply(
      X = strsplit(ratio, "_"),
      FUN = function(x) {
        a <- unlist(strsplit(x[[1]], "-"))
        b <- unlist(strsplit(x[[2]], "-"))
        expand.grid(b, a)
      }
    )
    edges <- do.call(rbind, edges)
    igraph::graph_from_data_frame(edges, directed = TRUE)
  }
)
