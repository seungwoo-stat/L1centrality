% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/L1centEDGE.R
\name{L1centEDGE}
\alias{L1centEDGE}
\alias{L1centEDGE.igraph}
\alias{L1centEDGE.matrix}
\alias{print.L1centEDGE}
\title{Multiscale Edge Representation}
\usage{
L1centEDGE(g, eta, alpha)

\method{L1centEDGE}{igraph}(g, eta = NULL, alpha)

\method{L1centEDGE}{matrix}(g, eta = NULL, alpha)

\method{print}{L1centEDGE}(x, ...)
}
\arguments{
\item{g}{An \code{igraph} graph object or a distance matrix. The graph must
be undirected and connected. Equivalently, the distance matrix must be
symmetric, and all entries must be finite.}

\item{eta}{An optional nonnegative multiplicity (weight) vector for (vertex)
weighted networks. The sum of its components must be positive. If set to
\code{NULL} (the default), all vertices will have the same positive weight
(multiplicity) of 1, i.e., \code{g} is treated as a vertex unweighted graph. The
length of the \code{eta} must be equivalent to the number of vertices.}

\item{alpha}{A number or a numeric vector of locality levels. Values
must be between 0 and 1.}

\item{x}{An \code{L1centEDGE} object, obtained as a result of the function
\code{L1centEDGE()}.}

\item{...}{Further arguments passed to or from other methods. This argument
is ignored here.}
}
\value{
\code{L1centEDGE()} returns an object of class \code{L1centEDGE}. It
is a list of \sQuote{edge lists}. The length of the list is equivalent to
the length of \code{alpha}, and the names of the list are the values of
\code{alpha}. The \code{i}th component of the list is a 2-column matrix,
and each row defines one directed edge, i.e., it is an edge list. The
second column is the local (level \code{alpha[i]}) median of the vertex at
the first column. There may be more than one edge from each vertex, since
there may be more than one local median.

\code{print.L1centEDGE()} prints the edge lists and returns them invisibly.
}
\description{
Derives a multiscale edge representation. Each vertex is connected to its
local median, which is found in its
\ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} centrality-based
neighborhood.
}
\details{
In a global perspective, any given undirected graph can be represented as a
star-shaped directed graph, with each vertex making a connection to the
median vertex. Based on this idea, an undirected graph can be represented as
a directed graph, with each vertex making a connection to the \emph{local}
median vertex. The local median vertex of, say, \eqn{v_i}, is defined as a
median vertex among the
\ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} centrality-based
neighborhood of \eqn{v_i}. By varying the level of locality, the given graph
can be visually inspected at multiple scales. Refer to Kang and Oh (2024) for
details.
}
\note{
The function is valid only for undirected and connected graphs.
}
\examples{
library(igraph)
MCU_edge <- L1centEDGE(MCUmovie, eta = V(MCUmovie)$worldwidegross, alpha = 5/32)
graph <- graph_from_edgelist(MCU_edge[[1]], directed = TRUE)
plot(graph)
}
\references{
S. Kang and H.-S. Oh. On a notion of graph centrality based on
\ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} data depth.
\emph{arXiv preprint arXiv:2404.13233}, 2024.
}
\seealso{
\code{\link[=L1cent]{L1cent()}}, \code{\link[=L1centNB]{L1centNB()}}, \code{\link[=L1centLOC]{L1centLOC()}}. Using the output, one can
use \code{\link[igraph:graph_from_edgelist]{igraph::graph_from_edgelist()}} for creating an \code{igraph} object.
See the example code below.

\link{Summary} for a relevant summary method.
}
