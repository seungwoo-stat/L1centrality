## igraph
validate_igraph <- function(g, checkdir = TRUE){
  if (checkdir & igraph::is_directed(g))
    stop("Directed graphs are not supported")
  if (!igraph::is_connected(g, mode = "strong"))
    stop("Disconnected graphs are not supported")
}

## distance matrix
validate_matrix <- function(g, eta, checkdir = TRUE){
  if (checkdir & !isSymmetric.matrix(g))
    stop("Distance matrix is non-symmetric")
  if(any(is.infinite(g)))
    stop("Disconnected graphs are not supported")
  if(length(eta) != ncol(g))
    stop("Length of eta differs from the number of vertices")
  if(any(eta < 0))
    stop("Entries of eta must be non-negative")
  if(sum(eta) <= 0)
    stop("sum(eta) must be positive")
}

## edge weight transformation
edge_weight_transform <- function(g, weight_transform){
  new_weight <- NULL
  if(is.null(weight_transform)){
    new_weight <- igraph::E(g)$weight
  }else if(!is.null(igraph::E(g)$weight)){
    new_weight <- weight_transform(igraph::E(g)$weight)
    if(length(new_weight) != length(igraph::E(g)$weight))
      stop("Length of edge weights after weight_transform differs from the original edge weight length")
  }
  return(new_weight)
}
