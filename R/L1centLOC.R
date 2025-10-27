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
#' details, refer to Kang and Oh (2025a) for undirected graphs, and Kang and Oh
#' (2025b) for directed graphs.
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
#' @examples
#' weight <- igraph::V(MCUmovie)$worldwidegross
#' MCUmovie_cent <- L1cent(MCUmovie, eta = weight)
#' MCUmovie_loc_cent <- L1centLOC(MCUmovie, eta = weight, alpha = 5/32)
#' plot(MCUmovie_cent, MCUmovie_loc_cent,
#'      main = "MCU movie network: global vs. local centrality")
#' @references S. Kang and H.-S. Oh. On a notion of graph centrality based on
#'   \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} data depth.
#'   \emph{Journal of the American Statistical Association}, 1--13, 2025a.
#'
#'   S. Kang and H.-S. Oh.
#'   \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} prominence measures
#'   for directed graphs. \emph{The American Statistician}, 1--16, 2025b.
L1centLOC <- function(g, eta, alpha, mode, weight_transform) UseMethod("L1centLOC")

#' @name L1centLOC
#' @exportS3Method L1centLOC igraph
L1centLOC.igraph <- function(g, eta = NULL, alpha, mode = c("centrality", "prestige"), weight_transform = NULL){
  validate_igraph(g, checkdir = FALSE)

  new_weight <- edge_weight_transform(g, weight_transform)
  if(!is.null(new_weight)) igraph::E(g)$weight <- new_weight
  D <- igraph::distances(g, mode = "out")
  attr(D, "label.igraph") <- igraph::V(g)$label
  L1centLOC.matrix(D, eta, alpha, mode)
}

#' @name L1centLOC
#' @exportS3Method L1centLOC matrix
L1centLOC.matrix <- function(g, eta = NULL, alpha, mode = c("centrality", "prestige"), weight_transform = NULL){
  if(is.null(eta)) eta <- rep(1,ncol(g))
  validate_matrix(g, eta, checkdir = FALSE)
  if(!all(alpha >= 0 & alpha <= 1))
    stop("alpha is not in the correct range: [0,1]")
  mode <- match.arg(tolower(mode), choices = c("centrality", "prestige"))
  label <- rownames(g)
  if(is.null(label)) label <- 1:nrow(g)

  if(identical(alpha, 1) | identical(alpha, 1L)){
    loc.cent <- list(c(L1cent(g, eta, mode)))
    names(loc.cent) <- alpha
    return(structure(loc.cent,
                     class = "L1centLOC",
                     mode = mode,
                     alpha = alpha,
                     label = label,
                     label.igraph = attr(g, "label.igraph")))
  }

  if(is.null(rownames(g))) rownames(g) <- colnames(g) <- 1:ncol(g)

  n <- ncol(g)
  sg <- ifelse(isSymmetric.matrix(g), 1,
               min((g/t(g))[upper.tri(g) | lower.tri(g)]))
  # m <- ceiling(n*alpha)
  # label <- colnames(g)
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
                   alpha = alpha,
                   label = label,
                   label.igraph = attr(g, "label.igraph")))
}

#' @name L1centLOC
#' @aliases print.L1centLOC
#'
#' @param x An \code{L1centLOC} object, obtained as a result of the function
#'   \code{L1centLOC()}.
#' @param ... Further arguments passed to or from other methods.
#' @export
print.L1centLOC <- function(x, ...){
  for(i in seq_along(x)){
    cat("local L1 ", attr(x, "mode"), " at alpha = ",
        round(attr(x, "alpha")[[i]], 4), ":\n", sep = "")
    print.default(c(x[[i]]), ...)
    if(i != length(x)) cat("\n")
  }
  return(invisible(x))
}

#' @name L1centLOC
#' @aliases plot.L1centLOC
#'
#' @param y An optional argument providing the coordinates for a scatter plot.
#'   It could be an object of class \code{L1cent} or \code{L1centLOC}, or a
#'   numeric vector.
#' @param add A logical value. This argument is considered only when drawing a Lorenz curve.
#'  * \code{TRUE}: add the Lorenz curve to an already existing plot.
#'  * \code{FALSE} (the default): draw the Lorenz curve to a new graphic device.
#' @param threshold A number between 0 and 1. Vertices that have their maximum
#'   and minimum local \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}}
#'   prominence value difference above the \code{threshold} are indicated in
#'   colored lines.
#' @return `plot.L1centLOC()` draws a following plot.
#'  * \code{y} is not supplied and \code{alpha} is of length one: A Lorenz curve (the group heterogeneity plot)
#'  and returns an invisible copy of a Gini coefficient (the group heterogeneity
#'  index). \code{threshold} is ignored.
#'  * \code{y} is supplied and \code{alpha} is of length one: A scatter plot of
#'  \code{x} versus \code{y}. \code{threshold} is ignored.
#'  * \code{alpha}'s length is larger than one: A plot of \code{alpha} versus
#'  local \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} prominence
#'  values (in a uniform margin) for each vertex. If \code{threshold} is set,
#'  vertices that have their maximum
#'   and minimum local \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}}
#'   prominence value difference above the \code{threshold} are indicated in
#'   colored lines. \code{y} is ignored.
#' @export
plot.L1centLOC <- function(x, y = NULL, add = FALSE, threshold = NULL, ...){
  args <- list(...)
  if(length(attr(x, "alpha")) == 1){
    if(is.null(y)){
      args$add <- add
      if(is.null(args$main)) args$main <- "Lorenz plot"
      do.call(Lorenz_plot, c(list(unlist(x)), args))
    }else{
      if(methods::is(y,"L1centLOC")){
        if(is.null(args$xlab)) args$xlab <- as.expression(bquote("Local" ~ L[1] ~ .(attr(x, "mode")) ~ "(alpha =" ~ .(attr(x, "alpha"))*")"))
        if(is.null(args$ylab)) args$ylab <- as.expression(bquote("Local" ~ L[1] ~ .(attr(y, "mode")) ~ "(alpha =" ~ .(attr(y, "alpha"))*")"))
        do.call(plot, c(list(as.numeric(unlist(x)), as.numeric(unlist(y))), args))
      }else if(methods::is(y,"L1cent")){
        if(is.null(args$xlab)) args$xlab <- as.expression(bquote("Local" ~ L[1] ~ .(attr(x, "mode")) ~ "(alpha =" ~ .(attr(x, "alpha"))*")"))
        if(is.null(args$ylab)) args$ylab <- as.expression(bquote(L[1] ~ .(attr(y, "mode"))))
        do.call(plot, c(list(as.numeric(unlist(x)), as.numeric(y)), args))
      }else{
        if(is.null(args$xlab)) args$xlab <- as.expression("Local" ~ bquote(L[1] ~ .(attr(x, "mode")) ~ "(alpha =" ~ .(attr(x, "alpha"))*")"))
        if(is.null(args$ylab)) args$ylab <- "y"
        do.call(plot, c(list(as.numeric(x), y), args))
      }
    }
  }else{
    x_unif <- x |> sapply(\(vec) stats::ecdf(vec)(vec))
    x_unif <- x_unif[,order(attr(x, "alpha"))]
    attr(x, "alpha") <- sort(attr(x, "alpha"))
    if(is.null(threshold)){
      if(is.null(args$xlim)) args$xlim <- range(attr(x, "alpha"))
      if(is.null(args$ylim)) args$ylim <- c(0, 1)
      if(is.null(args$xlab)) args$xlab <- "Locality level"
      if(is.null(args$ylab)) args$ylab <- as.expression(bquote("Local" ~ L[1] ~ .(attr(x, "mode"))~"(uniform margin)"))
      do.call(plot, c(list(NA), args))
      do.call(graphics::matlines, c(list(attr(x,"alpha"), t(x_unif)), args))
    }else{
      x_local_change <- apply(x_unif, 1, \(vec) max(vec) - min(vec))
      if(is.null(args$xlim)) args$xlim <- range(attr(x, "alpha")) + c(0, 0.1*diff(range(attr(x,"alpha"))))
      if(is.null(args$ylim)) args$ylim <- c(0, 1)
      if(is.null(args$xlab)) args$xlab <- "Locality level"
      if(is.null(args$ylab)) args$ylab <- as.expression(bquote("Local" ~ L[1] ~ .(attr(x, "mode"))~"(uniform margin)"))
      do.call(plot, c(list(NA), args))
      if(sum(x_local_change < threshold) > 0) do.call(graphics::matlines, c(list(attr(x,"alpha"), t(x_unif[x_local_change < threshold,,drop=FALSE]), col = "gray", lty = 1)))
      if(sum(x_local_change >= threshold) > 0){
        do.call(graphics::matlines, c(list(attr(x,"alpha"), t(x_unif[x_local_change >= threshold,,drop=FALSE]),
                                 col = grDevices::palette()[(1:sum(x_local_change >= threshold) - 1) %% 8 + 1],
                                 lty = 1, lwd = 2), args))
        lab.threshold <- NULL
        if(is.null(attr(x,"label.igraph"))){
          lab.threshold <- attr(x,"label")[which(x_local_change >= threshold)]
        }else{
          lab.threshold <- attr(x,"label.igraph")[which(x_local_change >= threshold)]
        }
        text(max(attr(x,"alpha")), x_unif[which(x_local_change >= threshold), ncol(x_unif)],
             labels = lab.threshold,
             pos = 4, col = grDevices::palette()[(1:sum(x_local_change >= threshold) - 1) %% 8 + 1])
      }
    }
  }
}
