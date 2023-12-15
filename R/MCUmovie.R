#' Marvel Cinematic Universe Movie Network
#'
#' @encoding UTF-8
#' @description
#'
#' Network between 32 movies from the Marvel Cinematic Universe (MCU) that were
#' released between 2008 and 2023. Each movie represents one vertex.
#'
#' An edge between movies
#' \ifelse{html}{\out{<i>i</i>}}{\eqn{i}} and
#' \ifelse{html}{\out{<i>j</i>}}{\eqn{j}} is formed if there is at least one
#' cast in common. Denoting the set of casts of movie \ifelse{html}{\out{<i>i</i>}}{\eqn{i}} as
#' \ifelse{html}{\out{<i>A<sub>i</sub></i>}}{\eqn{A_i}},
#' the weight of this edge is given as
#' \ifelse{html}{\out{(|<i>A<sub>i</sub></i>}\eqn{\cap}\out{<i>A<sub>j</sub></i>|/|<i>A<sub>i</sub></i>}\eqn{\cup}\out{<i>A<sub>j</sub></i>|)<sup>-1</sup>}}{\eqn{(|A_i\cap A_j|/|A_i\cup A_j|)^{-1}}},
#' where \eqn{|\cdot|} denotes the cardinality of a set.
#'
#' @name MCUmovie
#' @docType data
#' @keywords datasets
#' @usage data(MCUmovie)
#' @format An undirected, connected, and (edge) weighted \code{igraph} graph object with 32
#'   vertices and 278 edges.
#'
#'   Vertex attributes:
#'   \itemize{
#'    \item \sQuote{name}: name of the movie. e.g., \emph{Guardians
#'   of the Galaxy Vol. 3}.
#'    \item \sQuote{worldwidegross}: worldwide gross in USD. Archived from IMDb
#'    on Nov. 3rd, 2023.
#'    \item \sQuote{year}: release year of the movie.
#'   }
#'
#'   Edge attribute: \sQuote{weight}. Given as a dissimilarity between two
#'   vertices. See the description above.
#'
#' @source IMDb: https://www.imdb.com
#' @references G. Choi and H.-S. Oh. Heavy-snow transform: A new method for
#'   graph signals. Technical Report, 2021.
#'
#'   S. Kang and H.-S. Oh. On a notion of graph centrality based on
#'   \ifelse{html}{\out{<i>L<sub>1</sub></i>}}{{\eqn{L_1}}} data depth.
#'   Technical Report, 2023.
NULL

