#' @name L1centNB
#' @aliases L1presNB
#' @title L1 Centrality/Prestige-Based Neighborhood
#'
#' @description
#' Derives \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} centrality- or
#' \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} prestige-based
#' neighborhood of each vertex. For undirected graphs, the two neighborhood are
#' identical.
#'
#' @details
#' For an undirected graph, if the graph is symmetrized (in a way defined in Kang and Oh 2026a)
#' w.r.t. a vertex \ifelse{html}{\out{<i>v</i>}}{\eqn{v}}, vertex
#' \ifelse{html}{\out{<i>v</i>}}{\eqn{v}} becomes the graph median (Kang and Oh
#' 2026a), i.e., \ifelse{html}{\out{<i>v</i>}}{\eqn{v}} has
#' \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} centrality 1. Based on
#' this property, we define the
#' \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} centrality-based
#' neighborhood of vertex \ifelse{html}{\out{<i>v</i>}}{\eqn{v}} as vertices
#' that have large \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}}
#' centrality on the symmetrized graph w.r.t. vertex
#' \ifelse{html}{\out{<i>v</i>}}{\eqn{v}}.
#'
#' For a directed graph, a vertex of interest, say
#' \ifelse{html}{\out{<i>v</i>}}{\eqn{v}}, is made to a centrality and prestige
#' median vertex by the procedure described in Kang and Oh (2026b). We call the
#' resulting graph as the modified graph w.r.t.
#' \ifelse{html}{\out{<i>v</i>}}{\eqn{v}}.
#' \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}}
#' centrality (prestige) -based neighborhood of vertex
#' \ifelse{html}{\out{<i>v</i>}}{\eqn{v}} is a set of vertices that have large
#' \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} centrality (prestige)
#' on the modified graph w.r.t. vertex
#' \ifelse{html}{\out{<i>v</i>}}{\eqn{v}}.
#'
#' @note
#' The function is valid only for connected graphs. If the graph is directed, it
#' must be strongly connected.
#'
#' @inheritParams L1cent
#' @return \code{L1centNB()} returns an object of class \code{L1centNB}. It
#'   is a list of numeric vectors. The length of the list is
#'   equivalent to the number of vertices in the graph \code{g}, and the names of the
#'   list are vertex names. Each component of the list is a numeric vector whose
#'   length is equivalent to the number of vertices in the graph \code{g}.
#'   Specifically, the \code{i}th component of the list is a vector of the
#'   \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} centrality of each
#'   vertex, for the modified graph \code{g}
#'   w.r.t. the \code{i}th vertex (if \code{mode = "centrality"}) or the
#'   \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} prestige of each
#'   vertex, for the modified graph \code{g}
#'   w.r.t. the \code{i}th vertex (if \code{mode = "prestige"}).
#'
#'  `print.L1centNB()` prints
#'  \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} centrality or
#'  \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} prestige values at
#'  the modified graph w.r.t. each vertex and returns them invisibly.
#'
#' @export
#' @seealso [L1cent()] for
#'   \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} centrality/prestige,
#'   [L1centLOC()] and [L1centEDGE()] internally uses \code{L1centNB()}.
#
#' @examples
#' NB <- L1centNB(MCUmovie, vertex_weight = igraph::V(MCUmovie)$worldwidegross)
#' # Top 6 L1 centrality-based neighbors of "Iron Man"
#' utils::head(sort(NB$"Iron Man", decreasing = TRUE))
#' @references S. Kang and H.-S. Oh. On a notion of graph centrality based on
#'   \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} data depth.
#'   \emph{Journal of the American Statistical Association}, 121(553): 400--412, 2026a.
#'
#'   S. Kang and H.-S. Oh.
#'   \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} prominence measures
#'   for directed graphs. \emph{The American Statistician}, 80(2): 301--309, 2026b.
L1centNB <- function(g, vertex_weight, mode, edge_weight_transform) UseMethod("L1centNB")

#' @name L1centNB
#' @exportS3Method L1centNB igraph
L1centNB.igraph <- function(g, vertex_weight = NULL, mode = c("centrality", "prestige"), edge_weight_transform = NULL){
  validate_igraph(g, checkdir = FALSE)
  mode <- match.arg(tolower(mode), choices = c("centrality", "prestige"))

  new_weight <- edge_weight_transform0(g, edge_weight_transform)
  if(!is.null(new_weight)) igraph::E(g)$weight <- new_weight
  D <- igraph::distances(g, mode = "out")
  L1centNB.matrix(D, vertex_weight, mode = mode)
}

#' @name L1centNB
#' @exportS3Method L1centNB matrix
L1centNB.matrix <- function(g, vertex_weight = NULL, mode = c("centrality", "prestige"), edge_weight_transform = NULL){
  eta <- vertex_weight
  if(is.null(eta)) eta <- rep(1,ncol(g))
  validate_matrix(g, eta, checkdir = FALSE)
  mode <- match.arg(tolower(mode), choices = c("centrality", "prestige"))

  calls <- sys.calls()
  fnames <- sapply(calls, function(cl) as.character(cl[[1]]))
  # print(fnames)
  if(all(fnames[1:2] == c("L1centNB", "L1centNB.matrix")))
    message("DISTANCE matrix received")

  n <- ncol(g)
  sg <- ifelse(isSymmetric.matrix(g), 1,
               min((g/t(g))[upper.tri(g) | lower.tri(g)]))
  label <- colnames(g)
  etasum <- sum(eta)
  NB <- lapply(1:n, function(i){
    neweta <- eta
    neweta[i] <- etasum/sg + neweta[i]
    c(L1cent(g,vertex_weight=neweta,mode=mode))
  })
  names(NB) <- label
  structure(NB,
            class = "L1centNB",
            mode = mode)
}

#' @name L1centNB
#' @aliases print.L1centNB
#'
#' @param x An \code{L1centNB} object, obtained as a result of the function
#'   \code{L1cent()}.
#' @param ... Further arguments passed to or from other methods. This argument
#'   is ignored here.
#' @export
print.L1centNB <- function(x, ...){
  if(is.null(names(x))) names(x) <- paste0("V",seq_along(x))
  for(i in seq_along(x)){
    cat("L1 ", attr(x, "mode"), " in the modified graph w.r.t. ",
        sQuote(names(x)[[i]]), ":\n", sep = "")
    print.default(c(x[[i]]), ...)
    if(i != length(x)) cat("\n")
  }
  return(invisible(x))
}
