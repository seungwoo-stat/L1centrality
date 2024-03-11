#' @name L1centEDGE
#' @title Multiscale Edge Representation
#'
#' @description
#' Derives a multiscale edge representation. Each vertex is connected to its
#' local median, which is found in its
#' \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} centrality-based
#' neighborhood.
#'
#' @details
#' In a global perspective, any given graph can be represented as a star-shaped
#' (directed) graph, with each vertex making a connection to the median vertex.
#' Based on this idea, a graph can be represented as a directed graph, with each
#' vertex making a connection to the \emph{local} median vertex. The local
#' median vertex of, say, \eqn{v_i}, is defined as a median vertex among the
#' \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} centrality-based
#' neighborhood of \eqn{v_i}. By varying the level of locality, the given graph
#' can be visually inspected at multiple scales.
#'
#' @note
#' The function is valid only for undirected and connected graphs.
#'
#' @inheritParams L1centLOC
#' @param g An \code{igraph} graph object or a distance matrix. The graph must
#'   be undirected and connected. Equivalently, the distance matrix must be
#'   symmetric, and all entries must be finite.
#' @return A list of \sQuote{edge lists}. The length of the list is equivalent
#'   to the length of \code{alpha}, and the names of the list are the values of
#'   \code{alpha}. The \code{i}th component of the list is a 2-column matrix,
#'   and each row defines one directed edge, i.e., it is an edge list. The
#'   second column is the local (level \code{alpha[i]}) median of the vertex at
#'   the first column. There may be more than one edge from each vertex, since
#'   there may be more than one local median.
#'
#' @export
#' @seealso [L1cent()], [L1centNB()], [L1centLOC()]. Using the output, one can
#'   use [igraph::graph_from_edgelist()] for creating an \code{igraph} object.
#'   See the example code below.
#' @examples
#' library(igraph)
#' MCU_edge <- L1centEDGE(MCUmovie, eta = V(MCUmovie)$worldwidegross, alpha = 5/32)
#' graph <- graph_from_edgelist(MCU_edge[[1]], directed = TRUE)
#' plot(graph)
#' @references S. Kang and H.-S. Oh. On a notion of graph centrality based on
#'   \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} data depth.
#'   Manuscript, 2023.
L1centEDGE <- function(g, eta, alpha) UseMethod("L1centEDGE")

#' @name L1centEDGE
#' @exportS3Method L1centEDGE igraph
L1centEDGE.igraph <- function(g, eta=NULL, alpha){
  validate_igraph(g)

  D <- igraph::distances(g)
  L1centEDGE.matrix(D, eta, alpha)
}

#' @name L1centEDGE
#' @exportS3Method L1centEDGE matrix
L1centEDGE.matrix <- function(g, eta=NULL, alpha){
  if(is.null(eta)) eta <- rep(1,ncol(g))
  validate_matrix(g, eta)
  if(!all(alpha >= 0 & alpha <= 1))
    stop("alpha is not in the correct range: [0,1]")

  n <- ncol(g)
  label <- colnames(g)
  if(is.null(label))
    label <- 1:n

  if(identical(alpha, 1) | identical(alpha, 1L)){
    glob.median <- which(L1cent(g, eta) == 1)
    glob.median.length <- length(glob.median)
    glob.edgelist <- cbind(rep(1:n,each=glob.median.length),rep(glob.median,times=glob.median.length))
    rownames(glob.edgelist) <- NULL
    glob.edgelist <- matrix(label[glob.edgelist],ncol=2)
    glob.edgelist <- list(glob.edgelist)
    names(glob.edgelist) <- alpha
    return(glob.edgelist)
  }

  m <- ceiling(n*alpha)
  NB <- L1centNB(g)
  edgelist <- vector("list", length = length(alpha))
  names(edgelist) <- alpha

  for (i in seq_along(alpha)) {
    nb.index <-
      lapply(NB, function(l)
        which(l >= stats::quantile(l, 1 - m[i] / n)))
    loc.median <-
      lapply(1:length(nb.index), function(j){
        distsum <- colSums(g[nb.index[[j]],nb.index[[j]]] * eta[nb.index[[j]]])
        nb.index[[j]][which(distsum == min(distsum))]})
    loc.median.length <- sapply(loc.median, length)
    edgelist[[i]] <- cbind(rep(1:n, times = loc.median.length), unlist(loc.median))
    rownames(edgelist[[i]]) <- NULL
    edgelist[[i]] <- matrix(label[edgelist[[i]]],ncol=2)
  }
  return(edgelist)
}
