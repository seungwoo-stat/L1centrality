## usethis namespace: start
#' @importFrom graphics lines
#' @importFrom graphics text
#' @importFrom igraph distances
#' @importFrom Rcpp sourceCpp
#' @importFrom stats aggregate
#' @importFrom stats cmdscale
#' @importFrom stats dist
#' @importFrom stats quantile
#' @importFrom stats rnorm
#' @importFrom utils flush.console
#' @importFrom withr local_par
#' @useDynLib L1centrality, .registration = TRUE
## usethis namespace: end
NULL

#' L1centrality: Graph/Network Analysis Based on \out{<i>L</i><sub>1</sub>}
#' Centrality
#'
#' Analyze graph/network data using \out{<i>L</i><sub>1</sub>} centrality.
#' Functions for deriving global and local \out{<i>L</i><sub>1</sub>}
#' centralities and \out{<i>L</i><sub>1</sub>} centrality-based neighborhoods of
#' vertices are provided. Routines for visual inspection of a graph/network are
#' also provided.
#'
#' Every function inside this package supports a distinct variety of graphs.
#' Edge weights can be considered by all functions. Additionally, it is
#' assumed that the graph to be analyzed is a connected graph in the case of an
#' undirected graph, or a strongly connected graph in the case of a directed
#' graph.
#'
#' Certain functions exclusively accommodate undirected graphs without vertex
#' multiplicity, while others support both directed and
#' undirected graphs with vertex multiplicities. The following table provides a
#' concise overview of the support range for each function.
#'
#' \tabular{rccc}{
#'    \strong{Functions} \tab \strong{Undirected graph} \tab \strong{Directed graph} \tab \strong{Vertex multiplicity}\cr
#'    [L1cent()], [L1centNB()], [L1centLOC()] \tab O \tab O \tab O \cr
#'    [L1centEDGE()] \tab O \tab X \tab O \cr
#'    [L1centMDS()] \tab O \tab X \tab X
#' }
#'
#' @references S. Kang and H.-S. Oh. On a notion of graph centrality based on
#'   \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} data depth.
#'   Manuscript, 2023.
#' @docType package
#' @name L1centrality-package
#' @aliases L1centrality-package L1centrality
#' @keywords internal
"_PACKAGE"
