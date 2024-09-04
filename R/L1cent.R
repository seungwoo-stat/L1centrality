#' @name L1cent
#' @aliases L1prestige L1pres
#' @title L1 Centrality/Prestige
#'
#' @description
#' Computes \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} centrality or
#' \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} prestige for each
#' vertex. The \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}}
#' centrality/prestige is a graph centrality/prestige measure defined for the
#' vertices of a graph. It is (roughly) defined by (1 \eqn{-} minimum
#' multiplicity required for a selected vertex to become the median of the
#' graph). For directed graphs,
#' \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} centrality quantifies
#' the prominence of a vertex in *making* a choice and
#' \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} prestige quantifies
#' the prominence of a vertex in *receiving* a choice. For undirected graphs,
#' the two measures are identical.
#'
#' @note
#' The function is valid only for connected graphs. If the graph is directed, it
#' must be strongly connected.
#'
#' @details
#' Suppose that \code{g} is a (strongly) connected graph consisting of
#' \ifelse{html}{\out{<i>n</i>}}{\eqn{n}} vertices
#' \ifelse{html}{\out{<i>v</i><sub>1</sub>, ...,
#' <i>v<sub>n</sub></i>}}{{\eqn{v_1,\dots,v_n}}}
#' whose multiplicities (weights) are \eqn{\eta_1,\dots,\eta_n \geq 0}, respectively,
#' and \eqn{\eta_{\cdot} = \sum_{k=1}^n \eta_k > 0}.
#'
#' The centrality median vertex of this graph is the node minimizing the
#' weighted sum of distances. That is,
#' \ifelse{html}{\out{<i>v<sub>i</sub></i>}}{{\eqn{v_i}}} is the centrality
#' median vertex if
#' \deqn{
#'  \sum_{k=1}^{n} \eta_k d(v_i, v_k)
#' }
#' is minimized, where \eqn{d(v_i,v_k)} denotes the geodesic (shortest path)
#' distance from \eqn{v_i} to \eqn{v_k}. See [igraph::distances()] for
#' algorithms for computing geodesic distances between vertices. When the
#' indices are swapped to \eqn{d(v_k, v_i)} in the display above, we call the
#' node minimizing the weighted sum as the prestige median vertex. When the
#' graph is undirected, the prestige median vertex and the centrality median
#' vertex coincide, and we call it the graph median, following Hakimi (1964).
#'
#' The \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} centrality for an
#' arbitrary node \ifelse{html}{\out{<i>v<sub>i</sub></i>}}{{\eqn{v_i}}} is
#' defined as \sQuote{one minus the minimum weight that is required to make it a
#' centrality median vertex.} This concept of centrality is closely related to the
#' data depth for ranking multivariate data, as defined in Vardi and Zhang
#' (2000). It turns out that the following formula computes the
#' \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} centrality for the
#' vertex \ifelse{html}{\out{<i>v<sub>i</sub></i>}}{{\eqn{v_i}}}:
#' \deqn{
#'  1-\mathcal{S}(\texttt{g})\max_{j\neq i}\left\{\frac{\sum_{k=1}^{n}\eta_k (d(v_i,v_k) - d(v_j,v_k)) }{\eta_{\cdot}d(v_j,v_i)}\right\}^{+},
#' }
#' where \eqn{\{\cdot\}^{+}=\max(\cdot,0)} and \eqn{\mathcal{S}(\texttt{g}) =
#' \min_{i\neq j} d(v_i,v_j)/d(v_j,v_i)}. The
#' \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} centrality of a vertex
#' is in \eqn{[0,1]} by the triangle inequality, and the centrality median
#' vertex has centrality 1. The
#' \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} prestige is defined
#' analogously, with the indices inside the distance function swapped.
#'
#' For an undirected graph, \eqn{\mathcal{S}(\texttt{g}) = 1} since the distance
#' function is symmetric. Moreover,
#' \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} centrality and
#' \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} prestige measures concide.
#'
#' For details, refer to Kang and Oh (2024a) for undirected graphs, and Kang and
#' Oh (2024b) for directed graphs.
#'
#' @param g An \code{igraph} graph object or a distance matrix. The graph must
#'   be connected. For a directed graph, it must be strongly connected.
#'   Equivalently, all entries of the distance matrix must be finite. Here, the
#'   \ifelse{html}{\out{(<i>i,j</i>)}}{\eqn{(i,j)}} component of the distance
#'   matrix is the geodesic distance from the
#'   \ifelse{html}{\out{<i>i</i>}}{\eqn{i}}th vertex to the
#'   \ifelse{html}{\out{<i>j</i>}}{\eqn{j}}th vertex.
#' @param eta An optional nonnegative multiplicity (weight) vector for (vertex)
#'   weighted networks. The sum of its components must be positive. If set to
#'   \code{NULL} (the default), all vertices will have the same positive weight
#'   (multiplicity) of 1, i.e., \code{g} is treated as a vertex unweighted graph. The
#'   length of the \code{eta} must be equivalent to the number of vertices.
#' @param mode A character string. For an undirected graph, either choice gives
#'   the same result.
#'  * `centrality` (the default): \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}}
#'  centrality (prominence of each vertex in terms of \emph{making} a choice) is
#'  used for analysis.
#'  * `prestige`: \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}}
#'  prestige (prominence of each vertex in terms of \emph{receiving} a choice)
#'  is used for analysis.
#' @return A numeric vector whose length is equivalent to the number of vertices
#'   in the graph \code{g}. Each component of the vector is the
#'   \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} centrality (if
#'   \code{mode = "centrality"}) or the
#'   \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} prestige (if
#'   \code{mode = "prestige"}) of each vertex in the given graph.
#'
#' @export
#' @seealso [L1centLOC()], [L1centNB()], [L1centMDS()], [L1centEDGE()],
#'   [L1centGROUP()], [Lorenz_plot()] for
#'   \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} centrality- or
#'   prestige-based analysis. See [L1centrality-package] for each function's
#'   support range.
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
#'   \emph{arXiv preprint arXiv:2404.13233}, 2024a.
#'
#'   S. Kang and H.-S. Oh.
#'   \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} prominence measures
#'   for directed graphs. \emph{arXiv preprint arXiv:2408.12078}, 2024b.
#'
#'   Y. Vardi and C.-H. Zhang. The multivariate
#'   \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}}-median and
#'   associated data depth. \emph{Proceedings of the National Academy of Sciences},
#'   97(4):1423--1426, 2000.
L1cent <- function(g, eta, mode) UseMethod("L1cent")

#' @name L1cent
#' @exportS3Method L1cent igraph
L1cent.igraph <- function(g, eta = NULL, mode = c("centrality", "prestige")) {
  validate_igraph(g, checkdir = FALSE)
  mode <- match.arg(tolower(mode), choices = c("centrality", "prestige"))

  D <- igraph::distances(g, mode = "out")
  L1cent.matrix(g = D, eta = eta, mode = mode)
}

#' @name L1cent
#' @exportS3Method L1cent matrix
L1cent.matrix <- function(g, eta = NULL, mode = c("centrality", "prestige")) {
  if(is.null(eta)) eta <- rep(1,ncol(g))
  validate_matrix(g, eta, checkdir = FALSE)
  mode <- match.arg(tolower(mode), choices = c("centrality", "prestige"))

  n <- ncol(g)
  sg <- ifelse(isSymmetric.matrix(g), 1,
               min((g/t(g))[upper.tri(g) | lower.tri(g)]))

  if(identical(mode, "centrality")){
    geta1 <- matrix(rep(colSums(t(g) * eta), n), ncol = n)
    res <- rep(1, n) - sg / sum(eta) *
      apply((geta1 - t(geta1)) / (t(g) + diag(rep(Inf, n))), 1,
            function(r) max(c(r, 0)))
    pmin(pmax(res,0),1)
  }else{ # prestige
    geta2 <- matrix(rep(colSums(g * eta), n), ncol = n)
    res <- rep(1, n) - sg / sum(eta) *
      apply((geta2 - t(geta2)) / (g + diag(rep(Inf, n))), 1,
            function(r) max(c(r, 0)))
    pmin(pmax(res,0),1)
  }
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
#'   \emph{arXiv preprint arXiv:2404.13233}, 2024.
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
