#' @title Ternary 2d diagram of two rounds
#' @description Ternary 2d diagram of two rounds
#' @param data is the set of preference structures of each decision maker in round 1 and round 2
#' @param title is the title of the diagram
#' @param lines indicates whether to join the two preference structures of each decision maker by a line or not
#' @param labels is a vector with the labels of the preference structures of each decision maker
#' @param color indicates the colors of symbols
#' @param axes indicates whether to draw the axes or not
#' @param zones indicates whether to draw the zones or not
#' @param lobes indicates whether to draw the lobes or not
#' @param dens is a vector with the weights to scale round 2 according to reputation or not
#' @details This function computes the ternary 2d diagram of two rounds
#' @references Turón, A., Altuzarra, A. & Moreno-Jiménez, J.M. Visual Analytics Tools for Social Cognocracy Network.
#' In Linden, I., Mareschal, B., Liu, S., Papathanasiou, J., & Colot, C. (Eds.). (2017). Proceedings of the 2017
#' International Conference on Decision Support System Technology: Data, Information and Knowledge Visualisation
#' in Decision Making (Vol. 3). EWG-DSS.
#' @author Alberto Turón \email{turon@@unizar.es}
#' @seealso \code{\link{ternary_diagram}, \link{ternary_diagram_3d}, \link{ternary_diagram_3d_2r}}
#' @examples
#' w11 = c(0.39,0.12,0.46,0.64,0.26)
#' w12 = c(0.57,0.16,0.29,0.04,0.54)
#' w13 = c(0.04,0.73,0.25,0.32,0.20)
#' w21 = c(0.57,0.73,0.46,0.04,0.36)
#' w22 = c(0.39,0.16,0.25,0.34,0.40)
#' w23 = c(0.04,0.12,0.29,0.62,0.24)
#' dens = c(0.10,0.56,2.11,4.37,1.14)
#' dm = c('dm1','dm2','dm3','dm4','dm5')
#' data = data.frame(w11,w12,w13,w21,w22,w23)
#' ternary_diagram_2r(data)
#' ternary_diagram_2r(data,labels=dm)
#' ternary_diagram_2r(data,lines=TRUE)
#' @export
ternary_diagram_2r = function(data,labels=NULL,lines=FALSE,color=c(2,4,6),title=NULL,lobes=TRUE,zones=TRUE,axes=TRUE,dens=NULL) {

  graphics::plot.new()
  frame2d(title,lobes=lobes,zones=zones,axes=axes)

  numrows = nrow(data)

  data_t1 <- matrix(, nrow = numrows, ncol = 3)
  data_t2 <- matrix(, nrow = numrows, ncol = 3)

  for (i in 1:numrows) {
    data_t1[i,] = ilr(as.double(c(data[i,1],data[i,2],data[i,3])))
    data_t2[i,] = ilr(as.double(c(data[i,4],data[i,5],data[i,6])))
  }

  graphics::par(new=TRUE)

  plot(data_t1[,1:2],type="p", col=1,xlab="",ylab="", axes=FALSE, xlim=c(-0.75,0.75), ylim=c(-0.4,0.85), pch=21, bg=color[1])
  graphics::par(new=TRUE)
  if (is.null(dens))
    plot(data_t2[,1:2],type="p", col=1,xlab="",ylab="", axes=FALSE, xlim=c(-0.75,0.75), ylim=c(-0.4,0.85), pch=21, bg=color[2])
else
  plot(data_t2[,1:2],type="p", col=1,xlab="",ylab="", axes=FALSE, xlim=c(-0.75,0.75), ylim=c(-0.4,0.85), pch=21, bg=color[2],cex=dens)

  if (!is.null(labels)) {
    graphics::text(data_t1[,1:2]+c(0.06,0.06),labels=labels, cex=.7)
    graphics::text(data_t2[,1:2]+c(0.06,0.06),labels=labels, cex=.7)
  }

  if (lines) {
    for (i in 1:numrows) {
      if (!(is.na(data[i,1])*is.na(data[i,2])*is.na(data[i,3])*is.na(data[i,4])*is.na(data[i,5])*is.na(data[i,6]))) {
        d1 = clr(c(data[i,4],data[i,5],data[i,6]))
        d0 = clr(c(data[i,1],data[i,2],data[i,3]))
        v = (d1 - d0)
        line = matrix(, nrow=10, ncol=3)
        for (j in 1:10) {
          a = d0[1] + (j-1) * v[1] / 9
          b = d0[2] + (j-1) * v[2] / 9
          c = d0[3] + (j-1) * v[3] / 9
          line[j,] = ilr(clr_inv(c(a, b, c)))
        }
      }
      graphics::par(new=TRUE)
      plot(line[,1:2],type="l", col=color[3],xlab="",ylab="", axes=FALSE, xlim=c(-0.75,0.75), ylim=c(-0.4,0.85),lty = 3)
    }
  }

#  frame2d(title,lobes=lobes,zones=zones,axes=axes)
  graphics::legend(1.35,2.50,c("Round 1","Round 2"), pch=21, col=color[1], pt.bg=color, cex=0.7)
}
