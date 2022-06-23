#' @title Isometric 2d diagram of two rounds
#' @description Isometric 2d diagram of two rounds
#' @param data is the set of preference structures of each decision maker in round 1 and round 2
#' @param title is the title of the diagram
#' @param lines indicates whether to join the two preference structures of each decision maker by a line or not
#' @param labels is a vector with the labels of the preference structures of each decision maker
#' @param color indicates the colors of symbols
#' @param axes indicates whether to draw the axes or not
#' @param zones indicates whether to draw the zones or not
#' @param lobes indicates whether to draw the lobes or not
#' @param dens is a vector with the weights to scale round 2 according to reputation or not
#' @details This funcion computes the isometric 2d diagram of two rounds
#' @references Turón, A., Altuzarra, A. & Moreno-Jiménez, J.M. Visual Analytics Tools for Social Cognocracy Network.
#' In Linden, I., Mareschal, B., Liu, S., Papathanasiou, J., & Colot, C. (Eds.). (2017). Proceedings of the 2017
#' International Conference on Decision Support System Technology: Data, Information and Knowledge Visualisation
#' in Decision Making (Vol. 3). EWG-DSS.
#' @author Alberto Turón \email{turon@@unizar.es}
#' @seealso \code{\link{iso_diagram}, \link{iso_diagram_3d}, \link{iso_diagram_3d_2r}}
#' @examples
#' w11 = c(0.39,0.12,0.46,0.64,0.26)
#' w12 = c(0.57,0.16,0.29,0.04,0.54)
#' w13 = c(0.04,0.73,0.25,0.32,0.20)
#' w21 = c(0.57,0.73,0.46,0.04,0.36)
#' w22 = c(0.39,0.16,0.25,0.34,0.40)
#' w23 = c(0.04,0.12,0.29,0.62,0.24)
#' dm = c('dm1','dm2','dm3','dm4','dm5')
#' data = data.frame(w11,w12,w13,w21,w22,w23)
#' iso_diagram_2r(data)
#' iso_diagram_2r(data,labels=dm)
#' iso_diagram_2r(data,lines=TRUE)
#' @export
iso_diagram_2r = function(data,labels=NULL,lines=FALSE,color=c(2,4,6),title=NULL,axes=TRUE,zones=TRUE,lobes=TRUE,dens=NULL) {

  graphics::plot.new()
  rad = 2
  frame2d_iso(title,axes=axes,zones=zones,lobes=lobes,rad=rad)

  numrows = nrow(data)
  w11[] = data[,1]
  w12[] = data[,2]
  w13[] = data[,3]
  w21[] = data[,4]
  w22[] = data[,5]
  w23[] = data[,6]
  data_t1 <- matrix(, nrow = numrows, ncol = 3)
  data_t2 <- matrix(, nrow = numrows, ncol = 3)

  for (i in 1:numrows) {
    data_t1[i,] = rad * ilr(c(w11[i],w12[i],w13[i]))
    data_t2[i,] = rad * ilr(c(w21[i],w22[i],w23[i]))
  }

  graphics::par(new=TRUE)
  plot(data_t1[,1:2],type="p", asp=1,col=1,xlab="",ylab="", axes=FALSE, frame.plot=FALSE, xlim=c(-3,3), ylim=c(-3,3), pch=21, bg=color[1])
  graphics::par(new=TRUE)
  if (is.null(dens))
    plot(data_t2[,1:2],type="p", asp=1,col=1,xlab="",ylab="", axes=FALSE, frame.plot=FALSE, xlim=c(-3,3), ylim=c(-3,3), pch=21, bg=color[2])
  else
    plot(data_t2[,1:2],type="p", asp=1,col=1,xlab="",ylab="", axes=FALSE, frame.plot=FALSE, xlim=c(-3,3), ylim=c(-3,3), pch=21, bg=color[2],cex=dens)

  if (!is.null(labels)) {
    graphics::text(data_t1[,1:2]+c(0.2,0.2),labels=labels, cex=.7)
    graphics::text(data_t2[,1:2]+c(0.2,0.2),labels=labels, cex=.7)
  }

  # Líneas

  if (lines) {
    for (i in 1:numrows) {
      if (!(is.na(w11[i])*is.na(w12[i])*is.na(w13[i])*is.na(w21[i])*is.na(w22[i])*is.na(w23[i]))) {
        graphics::par(new=TRUE)
        plot(c(data_t1[i,1], data_t2[i,1]),c(data_t1[i,2], data_t2[i,2]) ,type="l", asp=1,col=color[3],xlab="",ylab="", axes=FALSE, xlim=c(-3,3), ylim=c(-3,3),lty = 3)
      }
    }
  }

  graphics::legend(1.35,2.7,c("Round 1","Round 2"), pch=21, col=1, pt.bg=c(color[1],color[2]), cex=0.6)

}
