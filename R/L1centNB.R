#' @name L1centNB
#' @aliases L1presNB
#' @title L1 Centrality/Prestige-Based Neighborhood
#'
#' @description
#' Derives \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}}
#' centrality- or prestige-based neighborhood of each vertex. For undirected graphs, the
#' two neighborhood are identical.
#'
#' @details
#' For an undirected graph, if the graph is symmetrized (in a way defined in Kang and Oh (2023))
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
#' For a directed graph, a vertex of interest becomes the graph centrality and
#' prestige median if the multiplicity of that vertex is incremented by
#' \eqn{\sum\eta_i/\mathcal{S}(G)}. We call the resulting graph as the
#' symmetrized graph, analogous to the symmetrization for undirected graphs.
#' Here, \eqn{\mathcal{S}(\texttt{g}) = \min_{i\neq j} d(v_i,v_j)/d(v_j,v_i)}.
#' \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}}
#' centrality(prestige)-based neighborhood of vertex
#' \ifelse{html}{\out{<i>v</i>}}{\eqn{v}} is a set of vertices that have large
#' \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} centrality(prestige)
#' on the symmetrized graph w.r.t. vertex  \ifelse{html}{\out{<i>v</i>}}{\eqn{v}}.
#'
#' @note
#' The function is valid only for connected graphs. If the graph is directed, it
#' must be strongly connected.
#'
#' @inheritParams L1cent
#' @return A list of numeric vectors. The length of the list is
#'   equivalent to the number of vertices in the graph \code{g}, and the names of the
#'   list are vertex names. Each component of the list is a numeric vector whose
#'   length is equivalent to the number of vertices in the graph \code{g}.
#'   Specifically, the \code{i}th component of the list is a vector of the
#'   \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} centrality of each
#'   vertex, for the symmetrized graph \code{g}
#'   w.r.t. the \code{i}th vertex (if \code{mode = "centrality"}) or the
#'   \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} prestige of each
#'   vertex, for the symmetrized graph \code{g}
#'   w.r.t. the \code{i}th vertex (if \code{mode = "prestige"}).
#'
#' @export
#' @seealso [L1cent()] for
#'   \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} centrality/prestige,
#'   [L1centLOC()] and [L1centEDGE()] internally uses \code{L1centNB()}.
#' @examples
#' NB <- L1centNB(MCUmovie, eta = igraph::V(MCUmovie)$worldwidegross)
#' # Top 6 L1 centrality-based neighbors of "Iron Man"
#' utils::head(sort(NB$"Iron Man", decreasing = TRUE))
#' @references S. Kang and H.-S. Oh. On a notion of graph centrality based on
#'   \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} data depth.
#'   Manuscript, 2023.
L1centNB <- function(g, eta, mode) UseMethod("L1centNB")

#' @name L1centNB
#' @exportS3Method L1centNB igraph
L1centNB.igraph <- function(g, eta = NULL, mode = c("centrality", "prestige")){
  validate_igraph(g, checkdir = FALSE)
  mode <- match.arg(tolower(mode), choices = c("centrality", "prestige"))

  D <- igraph::distances(g, mode = "out")
  L1centNB.matrix(D, eta, mode = mode)
}

#' @name L1centNB
#' @exportS3Method L1centNB matrix
L1centNB.matrix <- function(g, eta = NULL, mode = c("centrality", "prestige")){
  if(is.null(eta)) eta <- rep(1,ncol(g))
  validate_matrix(g, eta, checkdir = FALSE)
  mode <- match.arg(tolower(mode), choices = c("centrality", "prestige"))

  n <- ncol(g)
  sg <- ifelse(isSymmetric.matrix(g), 1,
               min((g/t(g))[upper.tri(g) | lower.tri(g)]))
  label <- colnames(g)
  etasum <- sum(eta)
  NB <- lapply(1:n, function(i){
    neweta <- eta
    neweta[i] <- etasum/sg + neweta[i]
    L1cent(g,eta=neweta,mode=mode)
  })
  names(NB) <- label
  return(NB)
}
