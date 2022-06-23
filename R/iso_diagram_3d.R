#' @title Isometric 3d diagram of one round
#' @description Isometric 3d diagram of one round
#' @param data is the set of preference structures of each decision maker
#' @param title is the title of the diagram
#' @param pov is the zenith angle for the viewer
#' @param valpha is the longitudinal angle for the viewer
#' @param labels is a vector with the labels of the preference structures of each decision maker
#' @param color indicates the colors of symbols
#' @param axes indicates whether to draw the axes or not
#' @param zones indicates whether to draw the zones or not
#' @param lobes indicates whether to draw the lobes or not
#' @details This function computes the isometric 2d diagram of one round
#' @references Turón, A., Altuzarra, A. & Moreno-Jiménez, J.M. Visual Analytics Tools for Social Cognocracy Network.
#' In Linden, I., Mareschal, B., Liu, S., Papathanasiou, J., & Colot, C. (Eds.). (2017). Proceedings of the 2017
#' International Conference on Decision Support System Technology: Data, Information and Knowledge Visualisation
#' in Decision Making (Vol. 3). EWG-DSS.
#' @author Alberto Turón \email{turon@@unizar.es}
#' @seealso \code{\link{iso_diagram}, \link{iso_diagram_2r}, \link{iso_diagram_3d_2r}}
#' @examples
#' w1 = c(0.25,0.11,0.29,0.31,0.28)
#' w2 = c(0.21,0.02,0.12,0.24,0.10)
#' w3 = c(0.24,0.26,0.17,0.44,0.18)
#' w4 = c(0.31,0.61,0.42,0.01,0.44)
#' dm = c('dm1','dm2','dm3','dm4','dm5')
#' data = data.frame(w1,w2,w3,w4)
#' iso_diagram_3d(data)
#' iso_diagram_3d(data,labels=dm)
#' iso_diagram_3d(data,lobes=FALSE)
#' iso_diagram_3d(data,axes=FALSE)
#' @export
iso_diagram_3d = function(data,title=NULL,labels=NULL,color=2,valpha=0.7,pov=0,zones=TRUE,lobes=TRUE,axes=TRUE) {

  Rad = 0.02
  rgl::open3d()

  if (pov == 1) {
    M=matrix(data=c(0.8093272,-0.5873579,-0.0005115378,0,0.4687286,0.6463909,-0.6020569205,0,0.3539538,0.4870214,0.7984529138,0,0.0000000,0.0000000,0.0000000000,1),nrow=4,byrow=TRUE)
    rgl::rgl.viewpoint(userMatrix=M)
  }
  else if (pov == 2) {
    #    view3d( theta = 120, phi = 30)
    M=matrix(data=c(-0.8093272,0.5873579,-0.0005115378,0,0.4687286,0.6463909,0.6020569205,0,0.3539538,0.4870214,-0.7984529138,0,0.000000000,0.00000000,0.000000000,1),nrow=4,byrow=TRUE)
    rgl::rgl.viewpoint(userMatrix=M)
  }
  else if (pov == 3) {
    #    view3d( theta = 210, phi = 30)
    M=matrix(data=c(0.7748804,0.3489503,-0.52706218,0,0.4910357,0.1927723,0.84954262,0,0.3980509,-0.9171005,-0.02197152,0,0.000000000,0.00000000,0.000000000,1),nrow=4,byrow=TRUE)
    rgl::rgl.viewpoint(userMatrix=M)
  }
  else if (pov == 4) {
    #    view3d( theta = -30, phi = 30)
    M=matrix(data=c(-0.02507478,-0.84756696,0.5300964117,0,0.01500852,0.52988440,0.8479371071,0,-0.99957299,0.02921775,-0.0005658192,0,0.000000000,0.00000000,0.000000000,1),nrow=4,byrow=TRUE)
    rgl::rgl.viewpoint(userMatrix=M)
  }

  frame3d_iso(title,zones=zones,lobes=lobes,axes=axes)

  numrows = nrow(data)
  w1[] = data[,1]
  w2[] = data[,2]
  w3[] = data[,3]
  w4[] = data[,4]

  data_t <- matrix(, nrow = numrows, ncol = 4)

  for (i in 1:numrows)
      data_t[i,] = ilr3d(c(w1[i],w2[i],w3[i],w4[i]))

  x <- data_t[,1]
  y <- data_t[,2]
  z <- data_t[,3]

  rgl::plot3d(x,y,z, col=color,type="s", radius=Rad, add=TRUE, box=FALSE, axes=FALSE, alpha=valpha, bg="#ff99ff")

  if (!is.null(labels))
    rgl::text3d(x+.02,y+.02,z+.02,labels, cex=.7)
}

