#' @title Inverse 3d isometric log-ratio transform
#' @description The function returns the inverse 3d isometric log-ratio transform of a vector \code{v}.
#' @param v an element or numeric 4d vector
#' @references Aitchison, J. (1986) The Statistical Analysis of Compositional Data, Monographs on Statistics and Applied Probability. Chapman & Hall Ltd., London (UK). 416p.
#' @author Alberto Turón \email{turon@@unizar.es}
#' @seealso \code{\link{clr}, \link{clr_inv}, \link{ilr}, \link{ilr_inv}, \link{ilr3d}}
#' @details This function computes the inverse 3d isomentric log-ratio transform of a vector \code{v}. It is used by the functions that create iso-diagrams.
#' @examples
#' \dontrun{ilr3d_inv(c(3,1,-2,2))}
ilr3d_inv <- function(v) {
  s4_3d <- matrix(c(1/sqrt(12),  1/sqrt(12),  1/sqrt(12), -3/sqrt(12), 1/sqrt(6),   1/sqrt(6),  -2/sqrt(6),   0.0,
                   1/sqrt(2),  -1/sqrt(2),   0.0,         0.0,   0.0,         0.0,         0.0,         1.0), nrow=4, byrow=T)

  return(solve(s4_3d) %*% v)
}
