#' @name L1cent
#' @aliases summary.L1cent
#' @param object An \code{L1cent} object, obtained as a result of the function
#'   \code{L1cent()}.
#' @return `summary.L1cent()` returns an object of class `table`.
#'   It is a summary of the prominence values with the five-number summary,
#'   mean, and the Gini coefficient.
#' @export
summary.L1cent <- function(object, ...){
  args <- list(...)
  structure(c(summary.default(object, ...),
              Gini = if(is.null(args$digits)) Gini(object) else signif(Gini(object), digits = args$digits)),
            class = "table")
}

#' @name L1centLOC
#' @aliases summary.L1centLOC
#' @param object An \code{L1centLOC} object, obtained as a result of the function
#'   \code{L1centLOC()}.
#' @return `summary.L1centLOC()` returns an object of class `table`.
#'   It is a summary of the prominence values with the five-number summary,
#'   mean, and the Gini coefficient, at each level of `alpha`.
#' @export
summary.L1centLOC <- function(object, ...){
  summat <- as.matrix(t(sapply(object, summary.L1cent, ...)))
  rownames(summat) <- round(attr(object, "alpha"), 4)
  names(dimnames(summat)) <- c("alpha", "")
  structure(summat, class = "table")
}

#' @name L1centNB
#' @aliases summary.L1centNB
#' @param object An \code{L1centNB} object, obtained as a result of the function
#'   \code{L1centNB()}.
#' @return `summary.L1centNB()` returns an object of class `table`.
#'   It is a summary of the prominence values with the five-number summary,
#'   mean, and the Gini coefficient, at each modified graph.
#' @export
summary.L1centNB <- function(object, ...){
  summat <- as.matrix(t(sapply(object, summary.L1cent, ...)))
  rownames(summat) <- names(object)
  names(dimnames(summat)) <- c("Modified w.r.t.", "")
  structure(summat, class = "table")
}

#' @name L1centEDGE
#' @aliases summary.L1centEDGE
#' @param object An \code{L1centEDGE} object, obtained as a result of the function
#'   \code{L1centEDGE()}.
#' @return `summary.L1centEDGE()` returns an object of class `table` with the
#'   number of local medians at each locality level `alpha`.
#' @export
summary.L1centEDGE <- function(object, ...){
  summat <- as.matrix(sapply(object, function(mat) length(unique(mat[,2]))))
  colnames(summat) <- "Number of local medians"
  names(dimnames(summat)) <- c("alpha","")
  structure(summat, class = "table")
}
