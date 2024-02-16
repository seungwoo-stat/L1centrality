#' @name L1centLOC
#' @title Local L1 Centrality
#'
#' @description
#' Computes local \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}}
#' centrality at each \code{alpha} level for every vertex.
#'
#' @note
#' The function is valid only for undirected and connected graphs.
#'
#' @details
#' Suppose that the given graph has \eqn{n} vertices. We choose about
#' \eqn{n\alpha} vertices
#' (\ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} centrality-based
#' neighborhood) for each vertex (see [L1centNB()]), and compute the
#' \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} centrality of the vertex
#' conditioned on these vertices, i.e., derive the
#' \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} centrality locally.
#'
#' @inheritParams L1cent
#' @param g An \code{igraph} graph object or a distance matrix. The graph must
#'   be undirected and connected. Equivalently, the distance matrix must be
#'   symmetric, and all entries must be finite.
#' @param alpha A number or a numeric vector of multiscale parameters. Values
#'   must be between 0 and 1.
#' @return A list of numeric vectors. The length of the list is equivalent to
#'   the length of \code{alpha}, and the names of the list are the values of
#'   \code{alpha}. Each component of the list is a numeric vector whose length
#'   is equivalent to the number of vertices in the graph \code{g}.
#'   Specifically, the \code{i}th component of the list is a vector of local
#'   \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} centrality at level
#'   \code{alpha[i]} for each vertex.
#'
#' @export
#' @seealso [L1cent()] for
#'   \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} centrality,
#'   [L1centNB()] for \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}}
#'   centrality-based neighborhood.
#' @examples
#' weight <- igraph::V(MCUmovie)$worldwidegross
#' MCUmovie_cent <- L1cent(MCUmovie, eta = weight)
#' MCUmovie_loc_cent <- L1centLOC(MCUmovie, eta = weight, alpha = 5/32)
#' plot(MCUmovie_cent, MCUmovie_loc_cent[[1]],
#'      xlab="Global L1 centrality", ylab="Local L1 centrality (alpha = 5/32)",
#'      main="MCU movie network: global vs. local centrality")
#' graphics::text(MCUmovie_cent, MCUmovie_loc_cent[[1]], igraph::V(MCUmovie)$name)
#' @references S. Kang and H.-S. Oh. On a notion of graph centrality based on
#'   \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} data depth.
#'   Manuscript, 2023.
L1centLOC <- function(g, eta, alpha) UseMethod("L1centLOC")

#' @name L1centLOC
#' @exportS3Method L1centLOC igraph
L1centLOC.igraph <- function(g, eta = NULL, alpha){
  validate_igraph(g)

  D <- igraph::distances(g)
  L1centLOC.matrix(D, eta, alpha)
}

#' @name L1centLOC
#' @exportS3Method L1centLOC matrix
L1centLOC.matrix <- function(g, eta = NULL, alpha){
  if(is.null(eta)) eta <- rep(1,ncol(g))
  validate_matrix(g, eta)
  if(!all(alpha >= 0 & alpha <= 1))
    stop("alpha is not in the correct range: [0,1]")

  if(identical(alpha, 1) | identical(alpha, 1L)){
    loc.cent <- list(L1cent(g, eta))
    names(loc.cent) <- alpha
    return(loc.cent)
  }

  if(is.null(rownames(g))) rownames(g) <- colnames(g) <- 1:ncol(g)

  n <- ncol(g)
  m <- ceiling(n*alpha)
  label <- colnames(g)
  NB <- L1centNB(g, eta = eta)
  loc.cent <- vector("list", length = length(alpha))
  names(loc.cent) <- alpha
  for (i in seq_along(alpha)) {
    nb.index <-
      lapply(NB, function(l)
        which(l >= stats::quantile(l, 1 - m[i] / n)))
    loc.cent[[i]] <-
      sapply(1:length(nb.index), function(j){
        index <- which(rownames(g.new <- g[nb.index[[j]], nb.index[[j]]]) == names(nb.index)[j])
        closenessinv <- colSums((eta.new <- eta[nb.index[[j]]])*g.new)
        1 - max((closenessinv[index] - closenessinv)/(g.new + diag(Inf,nrow(g.new)))[index,]/sum(eta.new))
      })
    names(loc.cent[[i]]) <- rownames(g)
  }
  return(loc.cent)
}


