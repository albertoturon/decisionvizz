#' @title Isometric 2d diagram of one round
#' @description Isometric 2d diagram of one round
#' @param data is the set of preference structures of each decision maker
#' @param title is the title of the diagram
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
#' @seealso \code{\link{iso_diagram_2r}, \link{iso_diagram_3d}, \link{iso_diagram_3d_2r}}
#' @examples
#' w1 = c(0.39,0.12,0.46,0.64,0.26)
#' w2 = c(0.57,0.16,0.29,0.04,0.54)
#' w3 = c(0.04,0.73,0.25,0.32,0.20)
#' dm = c('dm1','dm2','dm3','dm4','dm5')
#' data = data.frame(w1,w2,w3)
#' iso_diagram(data)
#' iso_diagram(data,labels=dm)
#' iso_diagram(data,lobes=FALSE)
#' iso_diagram(data,axes=FALSE)
#' @export
iso_diagram <- function(data,labels=NULL,color=2,title=NULL,axes=TRUE,zones=TRUE,lobes=TRUE) {

  graphics::plot.new()
  rad=2
  frame2d_iso(title,axes=axes,zones=zones,lobes=lobes,rad=rad)

  numrows = nrow(data)
  w1[] = data[,1]
  w2[] = data[,2]
  w3[] = data[,3]
  data_t <- matrix(, nrow = numrows, ncol = 3)

  for (i in 1:numrows)
    data_t[i,]=rad*ilr(c(w1[i],w2[i],w3[i]))

  graphics::par(new=TRUE)

  plot(data_t[,1:2],type="p", col=1,xlab="",ylab="", axes=FALSE, frame.plot=FALSE, xlim=c(-3,3), ylim=c(-3,3), pch=21, bg=color)

  if (!is.null(labels))
    graphics::text(data_t[,1:2]+c(0.2,0.2),labels=labels, cex=.7)
}
