# logical function for checking whether all elements of a vector are integral,
# from the documentation of `is.integer()`
# NOT exported
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

#' @name group_reduce
#' @title Group Reduced Graph
#'
#' @description
#' Computes the vertex multiplicities (weights) and the distance matrix of the
#' group reduced graph. The group reduced graph is constructed by replacing a
#' group of vertices in the original graph by a single \sQuote{pseudo-vertex}.
#'
#' @note
#' Multiple edges (edges with the same head and tail vertices) are not allowed,
#' because they make the edge weight setting procedure confusing.
#'
#' @details
#' The group reduced graph is constructed by replacing the vertices indicated in
#' the argument \code{nodes} with a single \sQuote{pseudo-vertex}. The
#' multiplicity (weight) of this new vertex is set to the sum of the
#' multiplicities of the vertices within \code{nodes}. An edge from the
#' pseudo-vertex to any vertex that is not in \code{nodes}, say
#' \ifelse{html}{\out{<i>v</i>}}{\eqn{v}}, is created in the group reduced graph if
#' there is at least one edge from the vertices in \code{nodes} to
#' \ifelse{html}{\out{<i>v</i>}}{\eqn{v}} in the original graph. The weight of
#' this newly added edge is determined using one of the following methods:
#' \itemize{
#' \item Minimum method: The edge weight from the pseudo-vertex to
#' \ifelse{html}{\out{<i>v</i>}}{\eqn{v}} is set to the minimum of the edge
#' weights of the edges between the vertices in \code{nodes} to
#' \ifelse{html}{\out{<i>v</i>}}{\eqn{v}} in the original graph.
#' \item Maximum method: The edge weight from the pseudo-vertex to
#' \ifelse{html}{\out{<i>v</i>}}{\eqn{v}} is set to the maximum of the edge
#' weights of the edges between the vertices in \code{nodes} to
#' \ifelse{html}{\out{<i>v</i>}}{\eqn{v}} in the original graph.
#' \item Average method: The edge weight from the pseudo-vertex to
#' \ifelse{html}{\out{<i>v</i>}}{\eqn{v}} is set to the average of the edge
#' weights of the edges between the vertices in \code{nodes} to
#' \ifelse{html}{\out{<i>v</i>}}{\eqn{v}} in the original graph.
#' }
#' An edge from \ifelse{html}{\out{<i>v</i>}}{\eqn{v}} to the pseudo-vertex is
#' set in a similar manner. For details, refer to Kang (2025).
#'
#' @inheritParams L1cent
#' @param g An \code{igraph} graph object or a distance matrix. Here, the
#'   \ifelse{html}{\out{(<i>i,j</i>)}}{\eqn{(i,j)}} component of the distance
#'   matrix is the geodesic distance from the
#'   \ifelse{html}{\out{<i>i</i>}}{\eqn{i}}th vertex to the
#'   \ifelse{html}{\out{<i>j</i>}}{\eqn{j}}th vertex.
#' @param nodes A vector of integers. Each integer indicates the index of the
#'   vertex.
#' @param method A character string. It specifies the method of setting the edge
#'   weight between the pseudo-vertex and the other vertices. Note that the S3
#'   method for the \code{matrix} class only supports the `minimum` option. This
#'   is because it is not possible to derive the group reduced graph's distance
#'   matrix from the original distance matrix when using the maximum or average
#'   method. On the other hand, the group reduced graph's distance matrix can be
#'   derived from the original distance matrix when the minimum method is used.
#'   See the discussion in Kang (2025).
#'  * `minimum` (the default): the minimum method is used in setting the edge weights.
#'  * `maximum`: the maximum method is used in setting the edge weights.
#'  * `average`: the average method is used in setting the edge weights.
#' @return A list consisting of three objects:
#'  \itemize{
#'  \item \sQuote{distmat}: A matrix representing the group reduced graph's
#'  distance matrix, where the first row and column correspond to the
#'  pseudo-vertex.
#'  \item \sQuote{eta}: A vector of the group reduced graph's vertex
#'  multiplicity. The first element corresponds to the pseudo-vertex.
#'  \item \sQuote{label}: A vector of the vertex names specified by \code{nodes}.
#' }
#'
#' @export
#' @seealso [L1cent()] for
#'   \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} centrality/prestige.
#'   [L1centGROUP()] internally uses \code{group_reduce()}.
#' @examples
#' # Group reduced graph of the 'Iron Man' series using the minimum method
#' vertex_weight <- igraph::V(MCUmovie)$worldwidegross
#' ironman_series <- c(1,3,7)
#' reduced_graph <- group_reduce(MCUmovie, nodes = ironman_series, eta = vertex_weight)
#' reduced_graph$distmat[1:3,1:3]
#' reduced_graph$label
#'
#' # Multiplicity of the pseudo-vertex equals the sums of the replaced vertices' multiplicities
#' reduced_graph$eta[1] == sum(vertex_weight[ironman_series])
#' @references S. Kang. \emph{Topics in Non-Euclidean Dimension Reduction}. PhD thesis,
#'   Seoul National University, 2025.
group_reduce <- function(g, nodes, eta, method) UseMethod("group_reduce")

#' @name group_reduce
#' @exportS3Method group_reduce igraph
group_reduce.igraph <- function(g, nodes, eta = NULL, method = c("minimum", "maximum", "average")){
  if(is.null(eta)) eta <- rep(1, igraph::vcount(g))
  # validate_igraph(g, checkdir = FALSE)
  if(igraph::any_multiple(g))
    stop("Multiple edges are not allowed")
  if(length(eta) != igraph::vcount(g))
    stop("Length of eta differs from the number of vertices")
  if(any(eta < 0))
    stop("Entries of eta must be non-negative")
  if(sum(eta) <= 0)
    stop("sum(eta) must be positive")

  n <- igraph::vcount(g)
  if(!all(is.wholenumber(nodes)) | !all(nodes >= 1 & nodes <= n))
    stop(paste0("Invalid 'nodes' argument: Elements should be integers between 1 and ",n))
  nodes <- sort(unique(nodes))
  method <- match.arg(tolower(method), choices = c("minimum", "maximum", "average"))

  if(is.null(igraph::E(g)$weight)) igraph::E(g)$weight <- rep(1, igraph::ecount(g))
  A <- igraph::as_adjacency_matrix(g, attr = "weight")
  if(is.null(colnames(A))) colnames(A) <- rownames(A) <- 1:n
  if(length(nodes) == n){
    Anew <- matrix(0)
  }else if(identical(method, "minimum")){
    A[A == 0] <- Inf
    Anew <- cbind(c(0,apply(A[-nodes, nodes, drop = FALSE], 1, min)),
                  rbind(apply(A[nodes, -nodes, drop = FALSE], 2, min), A[-nodes, -nodes, drop = FALSE]))
    Anew[Anew == Inf] <- 0.0
  }else if(identical(method, "maximum")){
    Anew <- cbind(c(0,apply(A[-nodes, nodes, drop = FALSE], 1, max)),
                  rbind(apply(A[nodes, -nodes, drop = FALSE], 2, max), A[-nodes, -nodes, drop = FALSE]))
  }else{ # average
    Anew <- cbind(c(0,apply(A[-nodes, nodes, drop = FALSE], 1, function(vec) ifelse(all(vec == 0), 0, mean(vec[vec != 0])))),
                  rbind(apply(A[nodes, -nodes, drop = FALSE], 2, function(vec) ifelse(all(vec == 0), 0, mean(vec[vec != 0]))), A[-nodes, -nodes, drop = FALSE]))
  }
  gnew <- igraph::graph_from_adjacency_matrix(Anew, weighted = TRUE)
  igraph::V(gnew)$name[1] <- "Pseudo-vertex"
  D <- igraph::distances(gnew, mode = "out")
  return(list(distmat = D, eta = c(sum(eta[nodes]), eta[-nodes]),
              label=if(is.null(igraph::V(g)$name)) nodes else igraph::V(g)$name[nodes]))
}

#' @name group_reduce
#' @exportS3Method group_reduce matrix
group_reduce.matrix <- function(g, nodes, eta = NULL, method = "minimum"){
  if(is.null(eta)) eta <- rep(1,ncol(g))
  # validate_matrix(g, eta, checkdir = FALSE)
  if(length(eta) != ncol(g))
    stop("Length of eta differs from the number of vertices")
  if(any(eta < 0))
    stop("Entries of eta must be non-negative")
  if(sum(eta) <= 0)
    stop("sum(eta) must be positive")

  n <- ncol(g)
  if(!all(is.wholenumber(nodes)) | !all(nodes >= 1 & nodes <= n))
    stop(paste0("Invalid nodes argument: Elements should be integers between 1 and ",n))
  nodes <- sort(unique(nodes))
  method <- match.arg(tolower(method), choices = "minimum")

  D <- cbind(c(0,apply(g[-nodes, nodes, drop = FALSE], 1, min)),
             rbind(apply(g[nodes, -nodes, drop = FALSE], 2, min), g[-nodes, -nodes]))
  colnames(D) <- rownames(D) <- c("Pseudo-vertex", if(is.null(colnames(g))) (1:n)[-nodes] else colnames(g)[-nodes])
  D[-1,-1] <- pmin(outer(D[-1,1],D[1,-1],"+"), D[-1,-1])
  return(list(distmat = D, eta = c(sum(eta[nodes]), eta[-nodes]),
              label=if(is.null(colnames(g))) nodes else colnames(g)[nodes]))
}


################################################################################

#' @name L1centGROUP
#' @aliases L1presGROUP
#' @title Group L1 Centrality/Prestige
#'
#' @description
#' Computes group \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}}
#' centrality or group \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}}
#' prestige for the specified group of vertices. For undirected graphs, the two
#' measures are identical.
#'
#' @note
#' The function is valid only for connected graphs. If the graph is directed, it
#' must be strongly connected. Multiple edges (edges with the same head and tail
#' vertices) are not allowed, because they make the edge weight setting procedure
#' confusing.
#'
#' @details
#' Given a group of vertices on a graph, we first construct a group reduced
#' graph by replacing the group of vertices by a single \sQuote{pseudo-vertex}
#' (see [group_reduce()] for the method of setting vertex multiplicities and
#' edge weights in the group reduced graph). The group
#' \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} centrality (prestige)
#' of this group is defined as the
#' \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} centrality (prestige)
#' of the pseudo-vertex in the group reduced graph.
#'
#' @inheritParams group_reduce
#' @param g An \code{igraph} graph object or a distance matrix. The graph must
#'   be connected. For a directed graph, it must be strongly connected.
#'   Equivalently, all entries of the distance matrix must be finite. Here, the
#'   \ifelse{html}{\out{(<i>i,j</i>)}}{\eqn{(i,j)}} component of the distance
#'   matrix is the geodesic distance from the
#'   \ifelse{html}{\out{<i>i</i>}}{\eqn{i}}th vertex to the
#'   \ifelse{html}{\out{<i>j</i>}}{\eqn{j}}th vertex.
#' @param mode A character string. For an undirected graph, either choice gives
#'   the same result.
#'  * `centrality` (the default): \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}}
#'  centrality (prominence of each vertex in terms of \emph{making} a choice) is
#'  used for analysis.
#'  * `prestige`: \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}}
#'  prestige (prominence of each vertex in terms of \emph{receiving} a choice)
#'  is used for analysis.
#' @return `L1centGROUP()` returns an object of class `L1centGROUP`. It is a
#'   numeric value of the group
#'  \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} centrality (if
#'  \code{mode = "centrality"}) or the group
#'  \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} prestige (if
#'  \code{mode = "prestige"}) of the specified group of vertices.
#'
#'  `print.L1centGROUP()` prints group
#'  \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} centrality or group
#'  \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} prestige value and
#'  returns it invisibly.
#'
#' @export
#' @seealso [L1cent()] for \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}}
#'   centrality/prestige, [group_reduce()] for details on the minimum, maximum, and average methods.
#' @examples
#' # Group L1 centrality of the 'Spider-Man' series
#' vertex_weight <- igraph::V(MCUmovie)$worldwidegross
#' L1centGROUP(MCUmovie, nodes = c(16,23,27), eta = vertex_weight)
#' @references S. Kang. \emph{Topics in Non-Euclidean Dimension Reduction}. PhD thesis,
#'   Seoul National University, 2025.
L1centGROUP <- function(g, nodes, eta, mode, method) UseMethod("L1centGROUP")

#' @name L1centGROUP
#' @exportS3Method L1centGROUP igraph
L1centGROUP.igraph <- function(g, nodes, eta = NULL, mode = c("centrality", "prestige"),
                               method = c("minimum", "maximum", "average")) {
  mode <- match.arg(tolower(mode), choices = c("centrality", "prestige"))
  method <- match.arg(tolower(method), choices = c("minimum", "maximum", "average"))
  validate_igraph(g, checkdir = FALSE)
  g_reduce <- group_reduce.igraph(g = g, nodes = nodes, eta = eta, method = method)
  D <- g_reduce$distmat
  eta <- g_reduce$eta
  n <- ncol(D)
  sg <- ifelse(isSymmetric.matrix(D), 1,
               min((D/t(D))[upper.tri(D) | lower.tri(D)]))
  res.value <- 0

  if(identical(mode, "centrality")){
    geta1 <- colSums(t(D) * eta)
    res <- 1 - sg / sum(eta) *
      max(c((geta1[1] - geta1[-1]) / D[-1,1]), 0)
    res.value <- min(max(res,0),1)
  }else{ # prestige
    geta2 <- colSums(D * eta)
    res <- 1 - sg / sum(eta) *
      max(c((geta2[1] - geta2[-1]) / D[1,-1]), 0)
    res.value <- min(max(res,0),1)
  }
  nodes <- sort(unique(nodes))
  return(structure(res.value,
                   class = c("L1centGROUP", "numeric"),
                   mode = mode,
                   label = g_reduce$label,
                   method = method))
}

#' @name L1centGROUP
#' @exportS3Method L1centGROUP matrix
L1centGROUP.matrix <- function(g, nodes, eta = NULL, mode = c("centrality", "prestige"),
                               method = "minimum") {
  mode <- match.arg(tolower(mode), choices = c("centrality", "prestige"))
  method <- match.arg(tolower(method), choices = c("minimum"))
  if(is.null(eta)) eta <- rep(1, ncol(g))
  validate_matrix(g, eta, checkdir = FALSE)
  g_reduce <- group_reduce.matrix(g = g, nodes = nodes, eta = eta, method = method)
  D <- g_reduce$distmat
  eta <- g_reduce$eta
  n <- ncol(D)
  sg <- ifelse(isSymmetric.matrix(D), 1,
               min((D/t(D))[upper.tri(D) | lower.tri(D)]))
  res.value <- 0

  if(identical(mode, "centrality")){
    geta1 <- colSums(t(D) * eta)
    res <- 1 - sg / sum(eta) *
      max(c((geta1[1] - geta1[-1]) / D[-1,1]), 0)
    res.value <- min(max(res,0),1)
  }else{ # prestige
    geta2 <- colSums(D * eta)
    res <- 1 - sg / sum(eta) *
      max(c((geta2[1] - geta2[-1]) / D[1,-1]), 0)
    res.value <- min(max(res,0),1)
  }
  nodes <- sort(unique(nodes))
  return(structure(res.value,
                   class = c("L1centGROUP", "numeric"),
                   mode = mode,
                   label = g_reduce$label,
                   method = method))
}

#' @name L1centGROUP
#' @aliases print.L1centGROUP
#'
#' @param x An \code{L1centGROUP} object, obtained as a result of the function
#'   \code{L1centGROUP()}.
#' @param ... Further arguments passed to or from other methods. This argument
#'   is ignored here.
#' @export
print.L1centGROUP <- function(x, ...){
  labs <- rep(" ", length(attr(x, "label")) * 2 - 1)
  labs[(1:length(attr(x, "label"))) * 2 - 1] <- sQuote(attr(x, "label"))
  labs[1] <- paste0("(", labs[1])
  labs[length(labs)] <- paste0(labs[length(labs)], ")")
  cat("group L1 ", attr(x, "mode"), " of ", length(attr(x, "label")),
      ifelse(length(attr(x, "label")) == 1, " vertex ", " vertices "),
      labs, " with ", sQuote(attr(x, "method")), " method:",
      sep = "", fill = TRUE)
  print.default(c(x))
  return(invisible(x))
}
