#' @title Ternary 2d diagram of one round
#' @description Ternary 2d diagram of one round
#' @param data is the set of preference structures of each decision maker in one round
#' @param title is the title of the diagram
#' @param labels is a vector with the labels of the preference structures of each decision maker
#' @param color indicates the colors of symbols
#' @param axes indicates whether to draw the axes or not
#' @param zones indicates whether to draw the zones or not
#' @param lobes indicates whether to draw the lobes or not
#' @param dens is a vector with the weights to scale the marks by the decision makers' reputation
#' @details This function computes the ternary 2d diagram of one round
#' @references Turón, A., Altuzarra, A. & Moreno-Jiménez, J.M. Visual Analytics Tools for Social Cognocracy Network.
#' In Linden, I., Mareschal, B., Liu, S., Papathanasiou, J., & Colot, C. (Eds.). (2017). Proceedings of the 2017
#' International Conference on Decision Support System Technology: Data, Information and Knowledge Visualisation
#' in Decision Making (Vol. 3). EWG-DSS.
#' @author Alberto Turón \email{turon@@unizar.es}
#' @seealso \code{\link{ternary_diagram_2r}, \link{ternary_diagram_3d}, \link{ternary_diagram_3d_2r}}
#' @examples
#' w1 = c(0.39,0.12,0.46,0.64,0.26)
#' w2 = c(0.57,0.16,0.29,0.04,0.54)
#' w3 = c(0.04,0.73,0.25,0.32,0.20)
#' dens = c(0.10,0.56,2.11,4.37,1.14)
#' dm = c('dm1','dm2','dm3','dm4','dm5')
#' data = data.frame(w1,w2,w3)
#' ternary_diagram(data)
#' ternary_diagram(data,labels=dm)
#' ternary_diagram(data,lobes=FALSE)
#' @export
ternary_diagram = function(data, labels=NULL, color=2,dens=NULL,title=NULL,lobes=TRUE,zones=TRUE,axes=TRUE) {

  graphics::plot.new()
  frame2d(title,lobes=lobes,zones=zones,axes=axes)

  numrows = nrow(data)
  data_t <- matrix(, nrow = numrows, ncol = 3)

  for (i in 1:numrows)
    data_t[i,] = ilr(as.double(data[i,]))

  graphics::par(new=TRUE)

  if (is.null(dens))
    plot(data_t[,1:2],type="p", col=1,xlab="",ylab="", axes=FALSE, xlim=c(-0.75,0.75), ylim=c(-0.4,0.85), pch=21, bg=color)
  else
    plot(data_t[,1:2],type="p", col=1,xlab="",ylab="", axes=FALSE, xlim=c(-0.75,0.75), ylim=c(-0.4,0.85), pch=21, bg=color, cex = 1+dens)

  if (!is.null(labels))
    graphics::text(data_t[,1:2]+c(0.06,0.06),labels=labels, cex=.7)
}
