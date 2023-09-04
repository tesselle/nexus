# GRAPH
#' @include AllGenerics.R
NULL

#' @export
#' @rdname as_graph
#' @aliases as_graph,LR-method
setMethod(
  f = "as_graph",
  signature = c(object = "LR"),
  definition = function(object) {
    ## Validation
    arkhe::needs("igraph")

    ratio <- object@ratio
    edges <- do.call(rbind, strsplit(ratio, "_"))
    edges <- edges[, c(2, 1)]
    igraph::graph_from_edgelist(edges, directed = FALSE)
  }
)

#' @export
#' @rdname as_graph
#' @aliases as_graph,ALR-method
setMethod(
  f = "as_graph",
  signature = c(object = "ALR"),
  definition = function(object) {
    ## Validation
    arkhe::needs("igraph")

    ratio <- object@ratio
    edges <- do.call(rbind, strsplit(ratio, "_"))
    edges <- edges[, c(2, 1)]
    igraph::graph_from_edgelist(edges, directed = TRUE)
  }
)

#' @export
#' @rdname as_graph
#' @aliases as_graph,ILR-method
setMethod(
  f = "as_graph",
  signature = c(object = "ILR"),
  definition = function(object) {
    ## Validation
    arkhe::needs("igraph")

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
