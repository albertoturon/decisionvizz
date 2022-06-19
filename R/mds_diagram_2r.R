#' @title Multidimensional scaling diagram of two rounds
#' @description Multidimensional scaling diagram of two rounds
#' @param data is the set of preference structures of each decision maker in two rounds
#' @param title is the title of the diagram
#' @param lines indicates whether to join the two preference structures of each decision maker by a line or not
#' @param labels is a vector with the labels of the preference structures of each decision maker or not
#' @param color indicates the colors of symbols
#' @param fill indicates whether to fill the symbols or not
#' @details This function computes the multidimensional scaling diagram (MDS) of two rounds
#' @references Turón, A., Altuzarra, A. & Moreno-Jiménez, J.M. Visual Analytics Tools for Social Cognocracy Network.
#' In Linden, I., Mareschal, B., Liu, S., Papathanasiou, J., & Colot, C. (Eds.). (2017). Proceedings of the 2017
#' International Conference on Decision Support System Technology: Data, Information and Knowledge Visualisation
#' in Decision Making (Vol. 3). EWG-DSS.
#' @author Alberto Turón \email{turon@@unizar.es}
#' @seealso \code{\link{mds_diagram}}
#' @examples
#' w11 = c(0.29,0.24,0.15,0.15,0.27)
#' w12 = c(0.24,0.22,0.05,0.20,0.20)
#' w13 = c(0.06,0.08,0.04,0.28,0.09)
#' w14 = c(0.13,0.18,0.27,0.18,0.31)
#' w15 = c(0.28,0.28,0.49,0.20,0.12)
#' w21 = c(0.09,0.54,0.35,0.45,0.27)
#' w22 = c(0.24,0.12,0.15,0.10,0.20)
#' w23 = c(0.16,0.08,0.14,0.28,0.09)
#' w24 = c(0.13,0.18,0.17,0.08,0.31)
#' w25 = c(0.38,0.08,0.19,0.09,0.13)
#' dm = c('dm1','dm2','dm3','dm4','dm5')
#' data = data.frame(w11,w12,w13,w14,w15,w21,w22,w23,w24,w25)
#' mds_diagram_2r(data)
#' mds_diagram_2r(data,labels=dm)
#' @export
mds_diagram_2r = function(data, title="MDS diagram",fill=TRUE,lines=FALSE,labels=NULL,color=c(1,2,4,"#ff99ff")) {
  nrow = nrow(data)
  nalt = ncol(data)/2
  ncol = 2*nalt

#  data = data[,1:ncol]
#  rownames(data)=data[[1]]

  prefsR1 = as.matrix(data[,1:nalt])
#  c1=nalt+2
  prefsR2 = as.matrix(data[,(nalt+1):ncol])

  alt = diag(nalt)
  totm = rbind(alt,round(prefsR1,digits=3), round(prefsR2,digits=3))

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

  # Points R1
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

  # Points R2
  for (i in 1:nrow) {
    vcols[i+nalt+nrow] = color[3]
    if (fill)
      vpichs[i+nalt+nrow] = 16
    else
      vpichs[i+nalt+nrow] = 1
    if (!is.null(labels))
      vlabels[i+nalt+nrow] = labels[i]
    else
      vlabels[i+nalt+nrow] = sprintf("dm%s",i)
  }

  pc=stats::cor(t(totm), method="p")
  mds=stats::cmdscale(stats::as.dist(1-pc),2)
  x1 <- mds[,1]
  y1 <- mds[,2]

  xmin = min(x1)
  xmax = max(x1)
  ymin = min(y1)
  ymax = max(y1)

  plot(mds, xlab="", ylab="", main=title, xlim = c(xmin-0.25,xmax+0.25), ylim = c(ymin-0.25,ymax+0.25), col=vcols, pch=vpichs)

  if (!is.null(labels))
    graphics::text(x1, y1+.07, labels = vlabels,  cex=.7)

  if (lines) {
    for (i in 1:nrow) {
      graphics::par(new=TRUE)
      plot(c(x1[i+nalt], x1[i+nalt+nrow]),c(y1[i+nalt], y1[i+nalt+nrow]),type="l", col=color[4],xlab="",ylab="", axes=FALSE, xlim = c(xmin-0.25,xmax+0.25), ylim = c(ymin-0.25,ymax+0.25),lty = 3)
    }
  }
  graphics::legend(0.18,1.12,c("Round 1","Round 2"), pch=21, col=color[1], pt.bg=color[2:3], cex=0.7)
}
