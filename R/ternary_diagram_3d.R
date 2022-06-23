#' @title Ternary 3d diagram of one round
#' @description Ternary 3d diagram of one round
#' @param data is the set of preference structures of each decision maker in one round
#' @param title is the title of the diagram
#' @param pov is the zenith angle for the viewer
#' @param valpha is the longitudinal angle for the viewer
#' @param labels is a vector with the labels of the preference structures of each decision maker
#' @param color indicates the color of symbols
#' @param axes indicates whether to draw the axes or not
#' @param zones indicates whether to draw the zones or not
#' @param lobes indicates whether to draw the lobes or not
#' @param planes indicates whether to draw the internal planes or not
#' @details This function computes the ternary 3d diagram of one round
#' @references Turón, A., Altuzarra, A. & Moreno-Jiménez, J.M. Visual Analytics Tools for Social Cognocracy Network.
#' In Linden, I., Mareschal, B., Liu, S., Papathanasiou, J., & Colot, C. (Eds.). (2017). Proceedings of the 2017
#' International Conference on Decision Support System Technology: Data, Information and Knowledge Visualisation
#' in Decision Making (Vol. 3). EWG-DSS.
#' @author Alberto Turón \email{turon@@unizar.es}
#' @seealso \code{\link{ternary_diagram}, \link{ternary_diagram_2r}, \link{ternary_diagram_3d_2r}}
#' @examples
#' w1 = c(0.25,0.11,0.29,0.31,0.28)
#' w2 = c(0.21,0.02,0.12,0.24,0.10)
#' w3 = c(0.24,0.26,0.17,0.44,0.18)
#' w4 = c(0.31,0.61,0.42,0.01,0.44)
#' dm = c('dm1','dm2','dm3','dm4','dm5')
#' data = data.frame(w1,w2,w3,w4)
#' ternary_diagram_3d(data)
#' ternary_diagram_3d(data,labels=dm)
#' ternary_diagram_3d(data,lobes=FALSE)
#' ternary_diagram_3d(data,pov=2)
#' @export
ternary_diagram_3d = function(data, title=NULL,labels=NULL, pov=0,valpha=0.7,zones=TRUE,lobes=TRUE,axes=TRUE,color=2,planes = FALSE) {

  Rad=0.02
  rgl::open3d()

  rgl::rgl.viewpoint(theta = 90, phi = 85, fov = 0)

  if (pov==1) {
    M=matrix(data=c(0.7926179,-0.6086854,-0.03548125,0,0.4303406,0.5997077,-0.67465413,0,0.4319305,0.5194738,0.73728073,0,0.0000000,0.0000000,0.0000000000,1),nrow=4,byrow=TRUE)
    rgl::rgl.viewpoint(userMatrix=M)
  }

  else if (pov==2) {
    M=matrix(data=c(-0.7913525,0.6104090,-0.03409106,0,0.4331504,0.5991551,0.67334539,0,0.4314422,0.5180871,-0.73854160,0,0.000000000,0.00000000,0.000000000,1),nrow=4,byrow=TRUE)
    rgl::rgl.viewpoint(userMatrix=M)
  }

  else if (pov==3) {
    M=matrix(data=c(0.7510887,0.4071710,-0.519690454,0,0.4560206,0.2492501,0.854353189,0,0.4774005,-0.8786848,0.001530549,0,0.000000000,0.00000000,0.000000000,1),nrow=4,byrow=TRUE)
    rgl::rgl.viewpoint(userMatrix=M)
  }

  else if (pov ==4) {
    M=matrix(data=c(-0.08026849,-0.86364806,0.497662783,0,0.03964655,0.49611267,0.867352545,0,-0.99598461,0.08935158,-0.005581504,0,0.000000000,0.00000000,0.000000000,1),nrow=4,byrow=TRUE)
    rgl::rgl.viewpoint(userMatrix=M)
  }

  frame3d(title,zones=zones,lobes=lobes,axes=axes,planes=planes)

  numrows = nrow(data)
  data_t <- matrix(, nrow = numrows, ncol = 4)

  for (i in 1:numrows)
    data_t[i,] = ilr3d(as.double(data[i,]))

  x <- data_t[,1]
  y <- data_t[,2]
  z <- data_t[,3]

  rgl::plot3d(x, y, z, col = color, type="s", radius=Rad, add=TRUE, box=FALSE, axes=FALSE, alpha=valpha, bg="#ff99ff")

  if (!is.null(labels))
    rgl::text3d(x+.02,y+.02,z+.02,labels, cex=.7)
}
