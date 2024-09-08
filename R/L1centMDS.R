#' @name L1centMDS
#' @title Fitting a Target Plot
#'
#' @description
#' \code{L1centMDS()} and \code{plot.L1centMDS()} are used together to draw a
#' target plot, which is a target-shaped 2D plot that aids in the visual
#' inspection of an undirected graph using the
#' \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} centrality. See Kang
#' and Oh (2024) for a formal definition of a target plot.
#'
#' @details
#' Denoting the \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}}
#' centrality of vertex \eqn{i} as \eqn{c_i\in(0,1]}, a point representing that vertex is placed
#' on a concentric circle with radius \eqn{r_i =
#' -\log(c_i)}. Representing each vertex as \eqn{(r_i, \theta_i)} (in circular
#' coordinates), the values of \eqn{\theta_i} are derived using nonmetric
#' multidimensional scaling proposed in Kruskal (1964a,b). The initial
#' configuration is derived using classical multidimensional scaling
#' ([stats::cmdscale()]). A gradient descent algorithm is employed in deriving
#' optimal \eqn{\theta_i}s.
#'
#' @note
#' The function \code{L1centMDS()} is valid only for undirected and connected
#' graphs. Also, \code{L1centMDS()} only considers graphs with equal vertex
#' multiplicities.
#'
#' @param g An \code{igraph} graph object or a distance matrix. The graph must
#'   be undirected and connected. Equivalently, the distance matrix must be
#'   symmetric, and all entries must be finite.
#' @param tol A numerical tolerance. The gradient descent method terminates if
#'   the relative magnitude of the gradient falls below \code{tol} as in Kruskal
#'   (1964b). By default set to \ifelse{html}{\out{10<sup>-5</sup>}}{\eqn{10^{-5}}}.
#' @param maxiter A number of maximum iteration allowances for the gradient descent
#'   algorithm. By default set to 1000.
#' @param x An \code{L1centMDS} object, obtained as a result of the function
#'   \code{L1centMDS()}.
#' @param verbose A boolean.
#'  * `TRUE` (the default): for each iteration, prints (1) current number of
#'  iterations, (2) current stress, and (3) relative magnitude of the gradient
#'  to the console. At the end, the final message is printed to the console; total
#'  number of iterations and final stress.
#'  * `FALSE`: suppress message to the console.
#' @param zoom A numerical value on how much to zoom-in the plot. By default set
#'   to 1 (no zoom).
#' @param main Title of the plot. If set to \code{NULL} (the default), the title
#'   prints \dQuote{Target plot / Stress = X}.
#' @param ... Further arguments passed to or from other methods.
#'   * `plot()` method: Further graphical parameters supplied to the internal
#'   [base::plot()] (for points) and [graphics::text()] (for labels) function.
#'   See [graphics::par()] document. To supply an argument to the former one,
#'   use the prefix \sQuote{\code{plot.}} and for the latter,
#'   \sQuote{\code{text.}}. For instance, \code{plot.cex = 1} sets the size of
#'   the point, whereas \code{text.cex = 1} sets the size of the label.
#'   * `print()` method: This argument is ignored.
#'
#' @return \code{L1centMDS()} returns an object of class \code{L1centMDS}. It is a list
#'   consisting of three vectors:
#' \itemize{
#'  \item \sQuote{radius}: Radius of a point representing each vertex in the
#'  target plot's circular coordinate system, i.e., \eqn{-\log(L_1\text{
#'  centrality})} for each vertex.
#'  \item \sQuote{theta}: Angle (in radians) of a point representing each vertex
#'  in the target plot's circular coordinate system.
#'  \item \sQuote{stress}: Stress measure defined in Kruskal (1964a).
#' }
#'
#' \code{plot.L1centMDS()} draws a target plot. Four concentric circles denote
#' the 1st to 4th quartiles of the radius, and the values of the
#' \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} centrality quartiles
#' are shown in red text. Note that red texts denote the
#' \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} centrality quartiles,
#' \emph{not} radius quartiles.
#'
#' `print.L1centMDS()` prints number of iterations it took to fit a target plot.
#'
#' @export
#' @seealso [L1cent()] for
#'   \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} centrality/prestige,
#'   [MASS::isoMDS()] and [stats::cmdscale()] for multidimensional scaling
#'   methods.
#' @examples
#' parameters <- L1centMDS(MCUmovie, verbose = FALSE)
#' plot(parameters)
#' @references S. Kang and H.-S. Oh. On a notion of graph centrality based on
#'   \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} data depth.
#'   \emph{arXiv preprint arXiv:2404.13233}, 2024.
#'
#'   J. B. Kruskal. Multidimensional scaling by optimizing goodness of fit to a
#'   nonmetric hypothesis. \emph{Psychometrika}, 29(1):1--27, 1964a.
#'
#'   J. B. Kruskal. Nonmetric multidimensional scaling: a numerical method.
#'   \emph{Psychometrika}, 29(2): 115--129, 1964b.
L1centMDS <- function(g, tol, maxiter, verbose) UseMethod("L1centMDS")

#' @name L1centMDS
#' @exportS3Method L1centMDS igraph
L1centMDS.igraph <- function(g, tol = 1e-5, maxiter = 1000, verbose = TRUE){
  validate_igraph(g)

  D <- igraph::distances(g)
  L1centMDS.matrix(D, tol, maxiter, verbose)
}

#' @name L1centMDS
#' @exportS3Method L1centMDS matrix
L1centMDS.matrix <- function(g, tol = 1e-5, maxiter = 1000, verbose = TRUE){
  eta <- rep(1,ncol(g))
  validate_matrix(g, eta)

  stepsize <- 0.2
  n <- ncol(g)
  label <- rownames(g)
  dist.original.vec <- g[upper.tri(g)]
  cent <- L1cent(g, eta = eta)
  radius <- c(-log(cent))

  # initialize using classical MDS
  init <- stats::cmdscale(g)
  temp <- t(init)-init[which.min(radius),]
  # add small errors for stability
  params <- c(atan2(temp[2,],temp[1,])) + stats::rnorm(n,0,(pi/32)^2)

  # iterate until convergence
  iter.count <- 1
  stress <- Inf
  while(TRUE){
    # current distance matrix
    DIST <- as.matrix(stats::dist(cbind(radius*cos(params),radius*sin(params))))
    DIST.vec <- DIST[upper.tri(DIST)]

    # estimate HAT.DIST
    dist.original.group <- match(dist.original.vec, sort(unique(dist.original.vec)))
    ngroup <- length(unique(dist.original.group))
    group.mean.n <- t(stats::aggregate(DIST.vec,by=list(dist.original.group),function(s)c(mean(s),length(s)))[,2])

    group.mean.n[1,] <- .dhatestimate(group.mean.n[2,],group.mean.n[1,])

    HAT.DIST.vec <- group.mean.n[1,dist.original.group]
    HAT.DIST <- matrix(0,nrow=n,ncol=n)
    HAT.DIST[upper.tri(HAT.DIST)] <- HAT.DIST.vec
    HAT.DIST <- HAT.DIST + t(HAT.DIST)

    # current stress
    Tstar <- sum(DIST.vec^2)
    Sstar <- sum((DIST.vec-HAT.DIST.vec)^2)
    new.stress <- sqrt(Sstar/Tstar)

    # gradient descent method
    R <- matrix(rep(radius,n),ncol=n)
    P <- matrix(rep(params,n),ncol=n)
    gradient <- sqrt(Tstar/Sstar)/Tstar*rowSums(
      (DIST*(1-Sstar/Tstar) - HAT.DIST)*R*t(R)*
        sin(P - t(P))/
        sqrt(R^2 + t(R^2) - 2*R*t(R)*cos(P-t(P))),na.rm=TRUE ## remove 0/0
    )
    mag.gradient <- sqrt(sum(gradient^2)/sum(radius^2))
    if(iter.count > 1){
      # Kruskal (1964b, p.121) uses complicated formula for determining the
      # stepsize. The formula is determined heuristically, based on his
      # numerical experimentation. Since we are in a different setting where
      # points are constrained to lie on a concentric circle, we use the
      # following simple formula for updating the stepsize.
      stepsize <- stepsize * 0.95
    }
    params <- (params - stepsize * gradient/mag.gradient) %% (2*pi)

    if(verbose){
      cat("\rITER ",iter.count," / Stress: ", new.stress," / ",mag.gradient,"\t\t\r",sep="")
      utils::flush.console()
    }

    if(mag.gradient <= tol){
      stress <- new.stress
      if(verbose){
        cat("\rConverged / Iteration: ",iter.count," / Stress: ",stress,"             \n",sep="")
        utils::flush.console()
      }
      break;
    }
    if(iter.count >= maxiter){
      stress <- new.stress
      if(verbose){
        cat("\rStopped after maximum iteration: ",maxiter," / Stress: ",stress,"             \n",sep="")
        utils::flush.console()
      }
      break;
    }
    stress <- new.stress
    iter.count <- iter.count + 1
  }
  return(structure(list(radius=radius,theta=params,stress=stress),
                   class="L1centMDS",
                   label=label,
                   iteration = iter.count))
}


#' @name L1centMDS
#' @aliases plot.L1centMDS
#' @export
plot.L1centMDS <- function(x,zoom=1,main=NULL,...){
  radius <- x$radius
  M.radius <- max(radius)
  theta <- x$theta

  args <- list(...)
  plot.args <- list()
  text.args <- list()
  if(!is.null(names(args))){
    plot.args <- args[startsWith(names(args), "plot.")]
    names(plot.args) <- substr(names(plot.args),6,nchar(names(plot.args)))
    text.args <- args[startsWith(names(args), "text.")]
    names(text.args) <- substr(names(text.args),6,nchar(names(text.args)))
  }
  if(is.null(plot.args$pch)) plot.args$pch <- 20
  if(is.null(text.args$cex)) text.args$cex <- 0.5
  if(is.null(text.args$labels)){
    if(is.null(attr(x, "label"))) text.args$labels <- 1:length(radius)
    else text.args$labels <- attr(x, "label")
  }

  withr::local_par(list(mar = c(1,1,4,1)+0.1))
  do.call(plot, c(list(radius*cos(theta),radius*sin(theta),
                       asp=1,xlim=c(-M.radius,M.radius)/zoom,ylim=c(-M.radius,M.radius)/zoom,
                       xlab="",ylab="",axes=FALSE,
                       main=ifelse(is.null(main),paste0("Target plot / Stress = ",x$stress),main)),
                  plot.args))
  do.call(graphics::text, c(list(radius*cos(theta),radius*sin(theta)),text.args))

  xx <- cos(seq(0,2*pi,length.out=200))
  yy <- sin(seq(0,2*pi,length.out=200))
  for(grid in stats::quantile(radius,c(0.25,0.5,0.75,1))){
    graphics::lines((grid)*xx,(grid)*yy,col="gray")
    graphics::text(grid*cos(3*pi/4),grid*sin(3*pi/4),round(exp(-grid),4),col="red",cex=0.5)
  }
}

#' @name L1centMDS
#' @aliases print.L1centMDS
#'
#' @export
print.L1centMDS <- function(x, ...){
  cat("Target plot fitted after ", attr(x, "iteration"), " iterations", sep = "")
  return(invisible(x))
}
