#' @name Heterogeneity
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
#' @return `Lorenz_plot()` draws a Lorenz curve (the group heterogeneity plot) and returns an
#'   invisible copy of a Gini coefficient (the group heterogeneity index).
#'
#'   `Gini()` returns a Gini coefficient.
#'
#' @export
#' @seealso Use the function with [L1cent()] or [L1centLOC()], and compare
#'   distributions of the centrality measurements across several groups and
#'   graphs. [Summary] methods in this package come with the Gini coefficient.
#'
#' @examples
#' vertex_weight <- igraph::V(MCUmovie)$worldwidegross
#' cent <- L1cent(MCUmovie, eta=vertex_weight)
#' gini <- Lorenz_plot(cent, asp=1)
#' graphics::abline(0,1,lty=2)
#' # group heterogeneity index
#' gini
#' gini == Gini(cent)
#' @references S. Kang and H.-S. Oh. On a notion of graph centrality based on
#'   \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} data depth.
#'   \emph{Journal of the American Statistical Association}, 1--13, 2025.
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


#' @name Heterogeneity
#' @aliases Gini
#' @export
Gini <- function(x){
  x <- sort(x)
  mu <- mean(x)
  n <- length(x)
  Fx <- c(0,seq(1/n, 1, length.out = n))
  Deltax <- c(0,1/mu*cumsum(x)/n)

  unname(1 - 2*(Fx[2])*(sum(Deltax)-1/2))
}
