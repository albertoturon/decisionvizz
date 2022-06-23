#' @title 2d isometric log-ratio transform
#' @description The function returns the 2d isometric log-ratio transform of a vector \code{v}.
#' @param v an element or numeric 3d vector
#' @details This function computes the 2d isometric log-ratio transform of a vector \code{v}. It is used by the functions that create iso-diagrams.
#' @references Aitchison, J. (1986) The Statistical Analysis of Compositional Data, Monographs on Statistics and Applied Probability. Chapman & Hall Ltd., London (UK). 416p.
#' @author Alberto Turón \email{turon@@unizar.es}
#' @seealso \code{\link{clr}, \link{clr_inv}, \link{ilr_inv}, \link{ilr3d}, \link{ilr3d_inv}}
ilr <- function(v) {
  s3_2d <- matrix(c(1/sqrt(6), 1/sqrt(6), -sqrt(2)/sqrt(3), 1/sqrt(2), -1/sqrt(2), 0, 0, 0, 1), nrow = 3, ncol = 3, byrow = TRUE)
  # Girada 90 grados CCW
  giro <- matrix(c(0,1,0,-1,0,0,0,0,1), nrow = 3, ncol = 3, byrow = TRUE)
  return(giro %*% s3_2d %*% v)
}
