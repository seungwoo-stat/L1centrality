#' @name L1centLOC
#' @aliases L1presLOC
#' @title Local L1 Centrality/Prestige
#'
#' @description
#' Computes local \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}}
#' centrality or local \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}}
#' prestige at each \code{alpha} level for every vertex. For undirected graphs,
#' the two measures are identical.
#'
#' @note
#' The function is valid only for connected graphs. If the graph is directed, it
#' must be strongly connected.
#'
#' @details
#' Suppose that the given graph has \ifelse{html}{\out{<i>n</i>}}{\eqn{n}}
#' vertices. We choose about \eqn{n\alpha} vertices
#' (\ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} centrality- or
#' \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} prestige-based
#' neighborhood) for each vertex (see [L1centNB()]), and compute the
#' \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} centrality or
#' \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} prestige of the vertex
#' conditioned on these vertices, i.e., derive the
#' \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} centrality or
#' \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} prestige locally. For
#' details, refer to Kang and Oh (2024a) for undirected graphs, and Kang and Oh
#' (2024b) for directed graphs.
#'
#' @inheritParams L1cent
#' @param alpha A number or a numeric vector of locality levels. Values
#'   must be between 0 and 1.
#' @return \code{L1centLOC()} returns an object of class \code{L1centLOC}. It is
#'   a list of numeric vectors. The length of the list is equivalent to the
#'   length of \code{alpha}, and the names of the list are the values of
#'   \code{alpha}. Each component of the list is a numeric vector whose length
#'   is equivalent to the number of vertices in the graph \code{g}.
#'   Specifically, the \code{i}th component of the list is a vector of local
#'   \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} centrality at level
#'   \code{alpha[i]} for each vertex (if \code{mode = "centrality"}) or local
#'   \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} prestige at level
#'   \code{alpha[i]} for each vertex (if \code{mode = "prestige"}).
#'
#'  `print.L1centLOC()` prints local
#'  \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} centrality or local
#'  \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} prestige values at
#'  each locality level `alpha` and returns them invisibly.
#'
#' @export
#' @seealso [L1cent()] for
#'   \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} centrality/prestige,
#'   [L1centNB()] for \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}}
#'   centrality/prestige-based neighborhood.
#'
#'   [Summary] for a relevant summary method.
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
#'   \emph{arXiv preprint arXiv:2404.13233}, 2024a.
#'
#'   S. Kang and H.-S. Oh.
#'   \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} prominence measures
#'   for directed graphs. \emph{arXiv preprint arXiv:2408.12078}, 2024b.
L1centLOC <- function(g, eta, alpha, mode) UseMethod("L1centLOC")

#' @name L1centLOC
#' @exportS3Method L1centLOC igraph
L1centLOC.igraph <- function(g, eta = NULL, alpha, mode = c("centrality", "prestige")){
  validate_igraph(g, checkdir = FALSE)

  D <- igraph::distances(g, mode = "out")
  L1centLOC.matrix(D, eta, alpha, mode)
}

#' @name L1centLOC
#' @exportS3Method L1centLOC matrix
L1centLOC.matrix <- function(g, eta = NULL, alpha, mode = c("centrality", "prestige")){
  if(is.null(eta)) eta <- rep(1,ncol(g))
  validate_matrix(g, eta, checkdir = FALSE)
  if(!all(alpha >= 0 & alpha <= 1))
    stop("alpha is not in the correct range: [0,1]")
  mode <- match.arg(tolower(mode), choices = c("centrality", "prestige"))

  if(identical(alpha, 1) | identical(alpha, 1L)){
    loc.cent <- list(c(L1cent(g, eta, mode)))
    names(loc.cent) <- alpha
    return(structure(loc.cent,
                     class = "L1centLOC",
                     mode = mode,
                     alpha = alpha))
  }

  if(is.null(rownames(g))) rownames(g) <- colnames(g) <- 1:ncol(g)

  n <- ncol(g)
  sg <- ifelse(isSymmetric.matrix(g), 1,
               min((g/t(g))[upper.tri(g) | lower.tri(g)]))
  # m <- ceiling(n*alpha)
  label <- colnames(g)
  NB <- L1centNB(g, eta = eta, mode)
  loc.cent <- vector("list", length = length(alpha))
  names(loc.cent) <- alpha
  for (i in seq_along(alpha)) {
    nb.index <-
      lapply(NB, function(l)
        which(l >= stats::quantile(l, 1 - alpha[i], type=1)))
    loc.cent[[i]] <-
      sapply(1:length(nb.index), function(j){
        index <- which(rownames(g.new <- g[nb.index[[j]], nb.index[[j]]]) == names(nb.index)[j])
        if(identical(mode, "centrality")) g.new <- t(g.new)
        closenessinv <- colSums((eta.new <- eta[nb.index[[j]]])*g.new)
        1 - sg*max((closenessinv[index] - closenessinv)/(g.new + diag(Inf,nrow(g.new)))[index,]/sum(eta.new))
      })
    names(loc.cent[[i]]) <- rownames(g)
  }
  return(structure(loc.cent,
                   class = "L1centLOC",
                   mode = mode,
                   alpha = alpha))
}

#' @name L1centLOC
#' @aliases print.L1centLOC
#'
#' @param x An \code{L1centLOC} object, obtained as a result of the function
#'   \code{L1centLOC()}.
#' @param ... Further arguments passed to or from other methods. This argument
#'   is ignored here.
#' @export
print.L1centLOC <- function(x, ...){
  for(i in seq_along(x)){
    cat("local L1 ", attr(x, "mode"), " at alpha = ",
        round(attr(x, "alpha")[[i]], 4), ":\n", sep = "")
    print.default(c(x[[i]]))
    cat("\n")
  }
  return(invisible(x))
}
