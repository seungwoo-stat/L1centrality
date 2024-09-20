#' @name Summary
#' @title Summary Methods in the L1centrality Package
#' @aliases summary.L1cent
#'
#' @description
#' `summary()` methods for the classes in the `L1centrality` package.
#'
#' @param object An object used to select a method.
#' @param ... Further arguments passed to or from other methods. This argument
#'   is ignored here.
#' @return For the methods for the class `L1cent`, `L1centLOC`, and `L1centNB`,
#'   object of class `summaryL1centrality` is returned. It is a summary of the
#'   prominence values with the five-number summary, mean, and the Gini
#'   coefficient.
#'
#'   For the method for the class `L1centEDGE`, number of local medians at each
#'   locality level `alpha` is returned.
#'
#' @examples
#' summary(L1cent(MCUmovie))
#' summary(L1centLOC(MCUmovie, alpha = c(5/32, 10/32)))
#' head(summary(L1centNB(MCUmovie)))
#' summary(L1centEDGE(MCUmovie, alpha = c(5/32, 10/32)))
#' @export
#' @seealso [L1cent()], [L1centLOC()], [L1centNB()], [L1centEDGE()], [Heterogeneity].
#'
#' @references S. Kang and H.-S. Oh. On a notion of graph centrality based on
#'   \ifelse{html}{\out{<i>L</i><sub>1</sub>}}{{\eqn{L_1}}} data depth.
#'   \emph{arXiv preprint arXiv:2404.13233}, 2024.
summary.L1cent <- function(object, ...){
  structure(c(summary.default(object), Gini = Gini(object)),
            class = "summaryL1centrality")
}

#' @name Summary
#' @aliases summary.L1centLOC
#' @export
summary.L1centLOC <- function(object, ...){
  summat <- as.matrix(t(sapply(object, summary.L1cent)))
  rownames(summat) <- round(attr(object, "alpha"), 4)
  names(dimnames(summat)) <- c("alpha", "")
  structure(summat, class = "summaryL1centrality")
}

#' @name Summary
#' @aliases summary.L1centNB
#' @export
summary.L1centNB <- function(object, ...){
  summat <- as.matrix(t(sapply(object, summary.L1cent)))
  rownames(summat) <- names(object)
  names(dimnames(summat)) <- c("Modified w.r.t.", "")
  structure(summat, class = "summaryL1centrality")
}

#' @name Summary
#' @aliases summary.L1centEDGE
#' @export
summary.L1centEDGE <- function(object, ...){
  summat <- as.matrix(sapply(object, function(mat) length(unique(mat[,2]))))
  colnames(summat) <- "Number of local medians"
  names(dimnames(summat)) <- c("alpha","")
  return(summat)
}
# number of local medians, names of the local medians

#' @name Summary
#' @aliases print.summaryL1centrality
#'
#' @param x A `summaryL1centrality` object, obtained as a result of the function
#'   `summary.L1cent()` or `summary.L1centLOC()` or `summary.L1centNB()`.
#' @param digits Minimal number of significant digits, see [print.default()].
#' @export
print.summaryL1centrality <- function(x, digits = max(3L, getOption("digits") - 3L), ...){
  print.table(x, digits = digits)
  invisible(x)
}
