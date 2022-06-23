#' @title 3d isometric log-ratio transform
#' @description The function returns the 3d isometric log-ratio transform of a vector \code{v}.
#' @param v an element or numeric 4d vector
#' @details This function computes the 3d isomentric log-ratio transform of a vector \code{v}. It is used by the functions that create iso-diagrams.
#' @references Aitchison, J. (1986) The Statistical Analysis of Compositional Data, Monographs on Statistics and Applied Probability. Chapman & Hall Ltd., London (UK). 416p.
#' @author Alberto Turón \email{turon@@unizar.es}
#' @seealso \code{\link{clr}, \link{clr_inv}, \link{ilr}, \link{ilr_inv}, \link{ilr3d_inv}}
ilr3d <- function(v) {
  s4_3d <- matrix(c(1/sqrt(12),  1/sqrt(12),  1/sqrt(12), -3/sqrt(12), 1/sqrt(6),   1/sqrt(6),  -2/sqrt(6),   0.0,
                   1/sqrt(2),  -1/sqrt(2),   0.0,         0.0,   0.0,         0.0,         0.0,         1.0), nrow=4, byrow=T)
  return(s4_3d %*% v)
}
