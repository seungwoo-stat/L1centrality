#' @name L1cent
#' @title L1 Centrality
#'
#' @description
#' Computes \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} centrality
#' for each vertex. The \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}}
#' centrality is a graph centrality measure defined for the vertices of a graph. It
#' is (roughly) defined by (1 \eqn{-} minimum multiplicity required for a selected
#' vertex to become the median of the graph).
#'
#' @note
#' The function is valid only for undirected and connected graphs.
#'
#' @details
#' Suppose that \code{g} is an undirected and connected graph consisting of
#' \eqn{n} vertices \eqn{v_1,\dots,v_n} whose multiplicities (weights) are
#' \eqn{\eta_1,\dots,\eta_n > 0}, respectively.
#'
#' The median of this graph is the node minimizing the weighted sum of distances
#' (Hakimi 1964). That is, \eqn{v_i} is the median node if
#' \deqn{
#'  \sum_{k=1}^{n} \eta_k d(v_i, v_k)
#' }
#' is minimized, where \eqn{d(\cdot,\cdot)} denotes the geodesic (shortest path)
#' distance between two vertices. See [igraph::distances()] for algorithms for
#' computing geodesic distances between vertices.
#'
#' The \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} centrality for an
#' arbitrary node \ifelse{html}{\out{<i>v<sub>i</sub></i>}}{{\eqn{v_i}}} is
#' defined as \sQuote{one minus the minimum weight that is required to make it a
#' median.} This concept of centrality is closely related to the data depth for
#' ranking multivariate data, as defined in Vardi and Zhang (2000). According to
#' Kang and Oh (2023), it turns out that the following formula computes the
#' \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} centrality for the
#' vertex
#' \ifelse{html}{\out{<i>v<sub>i</sub></i>}}{{\eqn{v_i}}}:
#' \deqn{
#'  1-\max_{j\neq i}\left\{\frac{\sum_{k=1}^{n}\eta_k (d(v_i,v_k) - d(v_j,v_k)) }{\eta_{\cdot}d(v_i,v_j)}\right\}^{+},
#' }
#' where \eqn{\{\cdot\}^{+}=\max(\cdot,0)} and \eqn{\eta_{\cdot} =
#' \sum_{k=1}^n \eta_k}. Hence, the
#' \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} centrality of a vertex
#' is in \eqn{[0,1]} by the triangle inequality, and the median vertex has
#' centrality 1.
#'
#' @param g An \code{igraph} graph object or a distance matrix. The graph must
#'   be undirected and connected. Equivalently, the distance matrix must be
#'   symmetric, and all entries must be finite.
#' @param eta An optional nonnegative multiplicity (weight) vector for (vertex)
#'   weighted networks. Sum of its components must be positive. If set to
#'   \code{NULL} (the default), all vertices will have the same positive weight
#'   (multiplicity), i.e., \code{g} is treated as a vertex unweighted graph. The
#'   length of the \code{eta} must be equivalent to the number of vertices.
#' @return A numeric vector whose length is equivalent to the number of vertices
#'   in the graph \code{g}. Each component of the vector is the
#'   \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} centrality of each
#'   vertex in the given graph.
#'
#' @export
#' @seealso [L1centLOC()], [L1centNB()], [L1centMDS()], [L1centEDGE()],
#'   [Lorenz_plot()] for \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}}
#'   centrality-based analysis.
#'
#'   [igraph::betweenness()], [igraph::closeness()],
#'   [igraph::degree()], [igraph::eigen_centrality()] for centrality measures.
#' @examples
#' # igraph object and distance matrix as an input lead to the same result
#' vertex_weight <- igraph::V(MCUmovie)$worldwidegross
#' cent_igraph <- L1cent(MCUmovie, eta=vertex_weight)
#' cent_matrix <- L1cent(igraph::distances(MCUmovie), eta=vertex_weight)
#' all(cent_igraph == cent_matrix)
#'
#' # Top 6 vertices with the highest L1 centrality
#' utils::head(sort(cent_igraph, decreasing = TRUE))
#' @references S. L. Hakimi. Optimum locations of switching centers and the
#'   absolute centers and medians of a graph. \emph{Operations Research},
#'   12(3):450--459, 1964.
#'
#'   S. Kang and H.-S. Oh. On a notion of graph centrality based on
#'   \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} data depth.
#'   Technical Report, 2023.
#'
#'   Y. Vardi and C.-H. Zhang. The multivariate
#'   \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}}-median and
#'   associated data depth. \emph{Proceedings of the National Academy of Sciences},
#'   97(4):1423--1426, 2000.
L1cent <- function(g, eta) UseMethod("L1cent")

#' @name L1cent
#' @exportS3Method L1cent igraph
L1cent.igraph <- function(g, eta = NULL) {
  validate_igraph(g)

  D <- igraph::distances(g)
  L1cent.matrix(D, eta)
}

#' @name L1cent
#' @exportS3Method L1cent matrix
L1cent.matrix <- function(g, eta = NULL) {
  if(is.null(eta)) eta <- rep(1,ncol(g))
  validate_matrix(g, eta)

  n <- ncol(g)
  geta1 <- matrix(rep(colSums(g *eta),n), ncol = n)
  res <- rep(1, n) - 1 / sum(eta) *
    apply((geta1 - t(geta1)) / (g + diag(rep(Inf, n))), 1,
          function(r) max(c(r, 0)))
  pmin(pmax(res,0),1)
}


#' @title Lorenz Curve and the Gini Coefficient
#'
#' @description
#' Draws a Lorenz curve (the group heterogeneity plot) and computes the Gini
#' coefficient (the group heterogeneity index).
#'
#' @param x A numeric vector.
#' @param add A logical value.
#'  * \code{TRUE}: add the Lorenz curve to an already existing plot.
#'  * \code{FALSE} (the default): draw the Lorenz curve to a new graphic device.
#' @param ... Further graphical parameters supplied to the internal
#'   [base::plot()] (when \code{add = FALSE}) or [graphics::lines()] (when
#'   \code{add = TRUE}) function. See [graphics::par()] document.
#' @return Draws a Lorenz curve (the group heterogeneity plot) and returns an
#'   invisible copy of a Gini coefficient (the group heterogeneity index).
#'
#' @export
#' @seealso Use the function with [L1cent()] or [L1centLOC()], and compare
#'   distributions of the centrality measurements across several groups and
#'   graphs.
#'
#' @examples
#' vertex_weight <- igraph::V(MCUmovie)$worldwidegross
#' cent <- L1cent(MCUmovie, eta=vertex_weight)
#' gini <- Lorenz_plot(cent, asp=1)
#' graphics::abline(0,1,lty=2)
#' # group heterogeneity index
#' gini
#' @references S. Kang and H.-S. Oh. On a notion of graph centrality based on
#'   \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} data depth.
#'   Technical Report, 2023.
#'
#'   M. O. Lorenz. Methods of measuring the concentration of wealth.
#'   \emph{Publications of the American Statistical Association}, 9(70):209--219, 1905.
Lorenz_plot <- function(x, add = FALSE, ...){
  x <- sort(x)
  mu <- mean(x)
  n <- length(x)
  Fx <- c(0,seq(1/n, 1, length.out = n))
  Deltax <- c(0,1/mu*cumsum(x)/n)
  if(!add){
    plot(Fx, Deltax, type="l", xlab="p",ylab="L(p)", ...)
  }else{
    graphics::lines(Fx, Deltax, ...)
  }
  invisible(unname(1 - 2*(Fx[2])*(sum(Deltax)-1/2))) # Gini index
}

#' #' @name Extract.L1cent
#' #' @title Extract or Replace Parts of a L1cent Object
#' #'
#' #' @description
#' #' Extract or Replace subsets of a L1cent Object
#' #' @param x A \code{L1cent} object.
#' #' @param ... A specification of indices -- see [base::Extract].
#' #' @return A \code{L1cent} object.
#' #' @export
#' #' @keywords internal
#' `[.L1cent` <- function(x, ...){
#'   structure(unclass(x)[...], class="L1cent")
#' }
