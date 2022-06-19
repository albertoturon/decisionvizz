#' @title Multidimensional scaling diagram
#' @description Multidimensional scaling diagram
#' @param data is the set of preference structures of each decision maker
#' @param title is the title of the diagram
#' @param labels is a vector with the labels of the preference structures of each decision maker
#' @param color indicates the colors of symbols
#' @param fill indicates whether to fill the symbols or not
#' @details This function computes the multidimensional scaling diagram (MDS) of one round
#' @references Turón, A., Altuzarra, A. & Moreno-Jiménez, J.M. Visual Analytics Tools for Social Cognocracy Network.
#' In Linden, I., Mareschal, B., Liu, S., Papathanasiou, J., & Colot, C. (Eds.). (2017). Proceedings of the 2017
#' International Conference on Decision Support System Technology: Data, Information and Knowledge Visualisation
#' in Decision Making (Vol. 3). EWG-DSS.
#' @author Alberto Turón \email{turon@@unizar.es}
#' @seealso \code{\link{mds_diagram_2r}}
#' @examples
#' w1 = c(0.29,0.24,0.15,0.15,0.27)
#' w2 = c(0.24,0.22,0.05,0.20,0.20)
#' w3 = c(0.06,0.08,0.04,0.28,0.09)
#' w4 = c(0.13,0.18,0.27,0.18,0.31)
#' w5 = c(0.28,0.28,0.49,0.20,0.12)
#' dm = c('dm1','dm2','dm3','dm4','dm5')
#' data = data.frame(w1,w2,w3,w4,w5)
#' mds_diagram(data)
#' @export
mds_diagram = function(data, title="MDS diagram",fill=TRUE,labels=NULL,color=c(1,2,4)) {
  nrow = nrow(data)
  nalt = ncol(data)
  ncol = 2*nalt
#  rownames(data)=data[[1]]

  # c1 = nalt+2
  #   data = data[,1:nalt+1]

  prefs = as.matrix(data)

  alt = diag(nalt)
  totm = rbind(alt, round(prefs,digits=3))

  vcols = c()
  vpichs = c()
  vlabels = c()

  # Alternatives
  for (i in 1:nalt) {
    vcols[i] = color[1]
    vpichs[i] = 8
    if (!is.null(colnames(data)))
       vlabels[i] = colnames(data)[i]
    else
      vlabels[i]=sprintf("A%s",i)
  }

  # Points
  for (i in 1:nrow) {
    vcols[i+nalt] = color[2]
    if (fill)
      vpichs[i+nalt] = 16
    else
      vpichs[i+nalt] = 1
    if (!is.null(labels))
      vlabels[i+nalt] = labels[i]
    else
      vlabels[i+nalt] = sprintf("dm%s",i)
  }

  pc=stats::cor(t(totm), method="p")
  mds=stats::cmdscale(stats::as.dist(1-pc),2)
  x1 <- mds[,1]
  y1 <- mds[,2]

  xmin = min(x1)
  xmax = max(x1)
  ymin = min(y1)
  ymax = max(y1)

  plot(mds, xlab="", ylab="", main=title, xlim = c(xmin,xmax), ylim = c(ymin,ymax), col=vcols, pch=vpichs)

#  if (!is.null(labels))
    graphics::text(x1, y1 + .05, labels = vlabels,  cex=.7)
}
