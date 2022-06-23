#' @title Inverse centered log-ratio transform
#' @description The function returns the inverse centered log-ratio transform of a vector \code{v}.
#' @param v an element or numeric vector
#' @details This function computes the inverse centered log-ratio transform of a vector \code{v}. It is used by the functions that create iso-diagrams.
#' @references Aitchison, J. (1986) The Statistical Analysis of Compositional Data, Monographs on Statistics and Applied Probability. Chapman & Hall Ltd., London (UK). 416p.
#' @author Alberto Turón \email{turon@@unizar.es}
#' @seealso \code{\link{clr}, \link{ilr}, \link{ilr_inv}, \link{ilr3d}, \link{ilr3d_inv}}
clr_inv <- function(v) {
  s <- 0
  for (i in 1:length(v))
    s <- s + exp(v[i])
  return(exp(v) / s)
}
