#' @name L1centNB
#' @title L1 Centrality-Based Neighborhood
#'
#' @description
#' Derives \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}}
#' centrality-based neighborhood of each vertex.
#'
#' @details
#' If a graph is symmetrized (in a way defined in Kang and Oh (2023))
#' w.r.t. a vertex \ifelse{html}{\out{<i>v</i>}}{\eqn{v}}, vertex
#' \ifelse{html}{\out{<i>v</i>}}{\eqn{v}} becomes the graph median (Kang and Oh,
#' 2023), i.e., \ifelse{html}{\out{<i>v</i>}}{\eqn{v}} has
#' \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} centrality 1. Based on
#' this property, we define the
#' \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} centrality-based
#' neighborhood of vertex \ifelse{html}{\out{<i>v</i>}}{\eqn{v}} as vertices
#' that have large \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}}
#' centrality on the symmetrized graph w.r.t. vertex
#' \ifelse{html}{\out{<i>v</i>}}{\eqn{v}}.
#'
#' @note
#' The function is valid only for undirected and connected graphs.
#'
#' @inheritParams L1cent
#' @return A list of numeric vectors. The length of the list is
#'   equivalent to the number of vertices in the graph \code{g}, and the names of the
#'   list are vertex names. Each component of the list is a numeric vector whose
#'   length is equivalent to the number of vertices in the graph \code{g}.
#'   Specifically, the \code{i}th component of the list is a vector of the
#'   \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} centrality of each
#'   vertex, for the symmetrized graph \code{g} w.r.t. the \code{i}th vertex.
#'
#' @export
#' @seealso [L1cent()] for
#'   \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} centrality,
#'   [L1centLOC()] and [L1centEDGE()] internally uses \code{L1centNB()}.
#' @examples
#' NB <- L1centNB(MCUmovie, eta = igraph::V(MCUmovie)$worldwidegross)
#' # Top 6 L1 centrality-based neighbors of "Iron Man"
#' utils::head(sort(NB$"Iron Man", decreasing = TRUE))
#' @references S. Kang and H.-S. Oh. On a notion of graph centrality based on
#'   \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} data depth.
#'   Technical Report, 2023.
L1centNB <- function(g, eta) UseMethod("L1centNB")

#' @name L1centNB
#' @exportS3Method L1centNB igraph
L1centNB.igraph <- function(g, eta = NULL){
  validate_igraph(g)

  D <- igraph::distances(g)
  L1centNB.matrix(D, eta)
}

#' @name L1centNB
#' @exportS3Method L1centNB matrix
L1centNB.matrix <- function(g, eta = NULL){
  if(is.null(eta)) eta <- rep(1,ncol(g))
  validate_matrix(g, eta)

  n <- ncol(g)
  label <- colnames(g)
  etasum <- sum(eta)
  NB <- lapply(1:n, function(i){
    neweta <- eta
    neweta[i] <- etasum + neweta[i]
    L1cent(g,eta=neweta)
  })
  names(NB) <- label
  return(NB)
}
