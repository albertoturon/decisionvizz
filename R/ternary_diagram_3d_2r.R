#' @title Ternary 3d diagram of two rounds
#' @description Ternary 3d diagram of two rounds
#' @param data is the set of preference structures of each decision maker in round 1 and round 2
#' @param title is the title of the diagram
#' @param lines indicates whether to join the two preference structures of each decision maker by a line or not
#' @param pov is the zenith angle for the viewer
#' @param valpha is the longitudinal angle for the viewer
#' @param labels is a vector with the labels of the preference structures of each decision maker
#' @param color indicates the colors of symbols
#' @param axes indicates whether to draw the axes or not
#' @param zones indicates whether to draw the zones or not
#' @param lobes indicates whether to draw the lobes or not
#' @param dens indicates whether to scale the marks by the decision makers' reputation
#' @details This function computes the ternary 2d diagram of two rounds
#' @references Turón, A., Altuzarra, A. & Moreno-Jiménez, J.M. Visual Analytics Tools for Social Cognocracy Network.
#' In Linden, I., Mareschal, B., Liu, S., Papathanasiou, J., & Colot, C. (Eds.). (2017). Proceedings of the 2017
#' International Conference on Decision Support System Technology: Data, Information and Knowledge Visualisation
#' in Decision Making (Vol. 3). EWG-DSS.
#' @author Alberto Turón \email{turon@@unizar.es}
#' @seealso \code{\link{ternary_diagram}, \link{ternary_diagram_2r}, \link{ternary_diagram_3d}}
#' @examples
#' w11 = c(0.25,0.11,0.29,0.31,0.28)
#' w12 = c(0.21,0.02,0.12,0.24,0.10)
#' w13 = c(0.24,0.26,0.17,0.44,0.18)
#' w14 = c(0.30,0.61,0.42,0.01,0.44)
#' w21 = c(0.21,0.12,0.42,0.61,0.08)
#' w22 = c(0.25,0.22,0.12,0.24,0.20)
#' w23 = c(0.31,0.05,0.17,0.14,0.18)
#' w24 = c(0.23,0.61,0.29,0.01,0.54)
#' dens = c(0.10,0.56,2.11,4.37,1.14)
#' dm = c('dm1','dm2','dm3','dm4','dm5')
#' data = data.frame(w11,w12,w13,w14,w21,w22,w23,w24)
#' ternary_diagram_3d_2r(data)
#' ternary_diagram_3d_2r(data,labels=dm)
#' ternary_diagram_3d_2r(data,lobes=FALSE)
#' ternary_diagram_3d_2r(data,lines=TRUE)
#' @export
ternary_diagram_3d_2r = function(data,title="Ternary diagram",labels=NULL,lines=FALSE,pov=0,valpha=0.7,dens=NULL,zones=TRUE,lobes=TRUE,axes=TRUE,color=c(2,4,6)) {

  Rad=0.02

  rgl::open3d()

  rgl::rgl.viewpoint( theta = 90, phi = 85, fov = 0)

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

  frame3d(title,zones=zones,lobes=lobes,axes=axes)
  numrows = nrow(data)

  data_t1 <- matrix(, nrow = numrows, ncol = 4)
  data_t2 <- matrix(, nrow = numrows, ncol = 4)

  for (i in 1:numrows) {
    data_t1[i,] = ilr3d(as.double(c(data[i,1],data[i,2],data[i,3],data[i,4])))
    data_t2[i,] = ilr3d(as.double(c(data[i,5],data[i,6],data[i,7],data[i,8])))
  }

  x1 <- data_t1[,1]
  y1 <- data_t1[,2]
  z1 <- data_t1[,3]
  x2 <- data_t2[,1]
  y2 <- data_t2[,2]
  z2 <- data_t2[,3]

  rgl::plot3d(x1,y1,z1, col=color[1],type="s", radius=Rad, add=TRUE, box=FALSE, axes=FALSE, alpha=valpha, bg="#ff99ff")
  if (is.null(dens))
    rgl::plot3d(x2,y2,z2, col=color[2],type="s", radius=Rad, add=TRUE, box=FALSE, axes=FALSE, alpha=valpha, bg="#ff99ff")
  else
    rgl::plot3d(x2,y2,z2, col=color[2],type="s", radius=Rad*dens, add=TRUE, box=FALSE, axes=FALSE, alpha=valpha, bg="#ff99ff")

  if (!is.null(labels)) {
    rgl::text3d(x1+.02,y1+.02,z1+.02,labels, cex=.7)
    rgl::text3d(x2+.02,y2+.02,z2+.02,labels, cex=.7)
  }

  if (lines) {
    for (i in 1:numrows) {
      if (!(is.na(data[i,1])*is.na(data[i,2])*is.na(data[i,3])*is.na(data[i,4])*is.na(data[i,5])*is.na(data[i,6])*is.na(data[i,7])*is.na(data[i,8]))) {
        d1 = clr(c(data[i,5],data[i,6],data[i,7],data[i,8]))
        d0 = clr(c(data[i,1],data[i,2],data[i,3],data[i,4]))
        v = (d1 - d0)
        line = matrix(, nrow=15, ncol=4)
        for (j in 1:15) {
          a = d0[1] + (j-1) * v[1] / 14
          b = d0[2] + (j-1) * v[2] / 14
          c = d0[3] + (j-1) * v[3] / 14
          d = d0[4] + (j-1) * v[4] / 14
          line[j,] = ilr3d(clr_inv(c(a, b, c, d)))
        }
      }
      rgl::plot3d(line[,1:3],type="l", col=color[3],add=TRUE, box=FALSE, axes=FALSE,lty=3)
    }
  }

  rgl::legend3d(0.15,0.95,c("Round 1","Round 2"), pch=21, col=1, pt.bg=c(color[1], color[2]), cex=0.9)
}
