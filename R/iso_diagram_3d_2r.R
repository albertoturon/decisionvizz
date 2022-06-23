#' @title Isometric 3d diagram of two rounds
#' @description Isometric 3d diagram of two rounds
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
#' @details This function computes the isometric 2d diagram of two rounds.
#' @references Turón, A., Altuzarra, A. & Moreno-Jiménez, J.M. Visual Analytics Tools for Social Cognocracy Network.
#' In Linden, I., Mareschal, B., Liu, S., Papathanasiou, J., & Colot, C. (Eds.). (2017). Proceedings of the 2017
#' International Conference on Decision Support System Technology: Data, Information and Knowledge Visualisation
#' in Decision Making (Vol. 3). EWG-DSS.
#' @author Alberto Turón \email{turon@@unizar.es}
#' @seealso \code{\link{iso_diagram}, \link{iso_diagram_2r}, \link{iso_diagram_3d}}
#' @examples
#' w11 = c(0.25,0.11,0.29,0.31,0.28)
#' w12 = c(0.21,0.02,0.12,0.24,0.10)
#' w13 = c(0.24,0.26,0.17,0.44,0.18)
#' w14 = c(0.30,0.61,0.42,0.01,0.44)
#' w21 = c(0.21,0.12,0.42,0.61,0.08)
#' w22 = c(0.25,0.22,0.12,0.24,0.20)
#' w23 = c(0.31,0.05,0.17,0.14,0.18)
#' w24 = c(0.23,0.61,0.29,0.01,0.54)
#' dm = c('dm1','dm2','dm3','dm4','dm5')
#' data = data.frame(w11,w12,w13,w14,w21,w22,w23,w24)
#' iso_diagram_3d_2r(data)
#' iso_diagram_3d_2r(data,labels=dm)
#' iso_diagram_3d_2r(data,lobes=FALSE)
#' iso_diagram_3d_2r(data,axes=FALSE)
#' @export
iso_diagram_3d_2r = function(data,title=NULL,labels=NULL,lines=FALSE,color=c(2,4,6),valpha=0.7,pov=0,zones=TRUE,lobes=TRUE,axes=TRUE) {

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
  w11[] = data[,1]
  w12[] = data[,2]
  w13[] = data[,3]
  w14[] = data[,4]
  w21[] = data[,5]
  w22[] = data[,6]
  w23[] = data[,7]
  w24[] = data[,8]

  data_t1 <- matrix(, nrow = numrows, ncol = 4)
  data_t2 <- matrix(, nrow = numrows, ncol = 4)

  for (i in 1:numrows) {
    data_t1[i,] = ilr3d(c(w11[i],w12[i],w13[i],w14[i]))
    data_t2[i,] = ilr3d(c(w21[i],w22[i],w23[i],w24[i]))
  }

  x1 <- data_t1[,1]
  y1 <- data_t1[,2]
  z1 <- data_t1[,3]

  x2 <- data_t2[,1]
  y2 <- data_t2[,2]
  z2 <- data_t2[,3]

  rgl::plot3d(x1,y1,z1, col=color[1],type="s", radius=Rad, add=TRUE, box=FALSE, axes=FALSE, alpha=valpha, bg="#ff99ff")
  rgl::plot3d(x2,y2,z2, col=color[2],type="s", radius=Rad, add=TRUE, box=FALSE, axes=FALSE, alpha=valpha, bg="#ff99ff")

  if (!is.null(labels)) {
    rgl::text3d(x1+.02,y1+.02,z1+.02,labels, cex=.7)
    rgl::text3d(x2+.02,y2+.02,z2+.02,labels, cex=.7)
  }

  if (lines) {
    for (i in 1:numrows) {
      if (!(is.na(w11[i])*is.na(w12[i])*is.na(w13[i])*is.na(w14[i])*is.na(w21[i])*is.na(w22[i])*is.na(w23[i])*is.na(w24[i]))) {
        linex = c(data_t1[i,1],data_t2[i,1])
        liney = c(data_t1[i,2],data_t2[i,2])
        linez = c(data_t1[i,3],data_t2[i,3])

        rgl::plot3d(linex,liney,linez,type="l", col=color[3],add=TRUE, box=FALSE, axes=FALSE,lty=3)
      }
    }
  }

  rgl::legend3d(0.15,0.95,c("Round 1","Round 2"), pch=21, col=1, pt.bg=c(color[1], color[2]), cex=0.9)
}
