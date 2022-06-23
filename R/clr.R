#' @title Centered log-ratio transform
#' @description The function returns the log-ratio transform of a vector \code{v}.
#' @param v an element or numeric vector
#' @details This function computes the log-ratio transform of a vector \code{v}. It is used by the functions that create iso-diagrams.
#' @references Aitchison, J. (1986) The Statistical Analysis of Compositional Data, Monographs on Statistics and Applied Probability. Chapman & Hall Ltd., London (UK). 416p.
#' @author Alberto Turón \email{turon@@unizar.es}
#' @seealso \code{\link{clr_inv}, \link{ilr}, \link{ilr_inv}, \link{ilr3d}, \link{ilr3d_inv}}
clr <- function(v) {
  p <- 1
  for (i in 1:length(v))
    p <- p * v[i]
  p <- p ^ (1/length(v))
  return(log(v / p))
}
