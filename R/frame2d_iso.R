#' @title Frame for 2D iso diagrams
#' @description The function creates the frame for a 2D iso diagram.
#' @param title is the title of the diagram
#' @param labels are the labels of the three axes
#' @param rad is a scale factor generic for the diagram
#' @param axes indicates whether to draw the axes or not
#' @param zones indicates whether to draw the zones or not
#' @param lobes indicates whether to draw the lobes or not
#' @details This function creates the frame for a 2D iso diagram. It is used by the functions that create 2D iso diagrams.
#' @author Alberto Turón \email{turon@@unizar.es}
#' @seealso \code{\link{frame2d}, \link{frame3d}, \link{frame3d_iso}}
frame2d_iso <- function(title=NULL, labels=c("A1","A2","A3"),rad=rad,axes=TRUE,zones=TRUE,lobes=TRUE) {

  graphics::plot(0, type="n", main="", xlab="", ylab="", asp=1,frame.plot=FALSE, axes=FALSE, xlim=c(-3,3), ylim=c(-3,3))

#  rad = 2

  if (axes==TRUE) {
    a1 = c(-rad,0)
    a2 = c(rad,0)
    b1 = c(0, -rad)
    b2 = c(0,rad)
    graphics::par(new=TRUE)
    plot(c(a1[1],a2[1]),c(a1[2],a2[2]), type="l", asp=1,col="#999999", main=title, xlab="",ylab="", axes=FALSE, frame.plot=FALSE, xlim=c(-3,3), ylim=c(-3,3))
    graphics::par(new=TRUE)
    plot(c(b1[1],b2[1]),c(b1[2],b2[2]), type="l", asp=1,col="#999999", main=title, xlab="",ylab="", axes=FALSE, frame.plot=FALSE, xlim=c(-3,3), ylim=c(-3,3))

    O = c( 0,                0)
    a = c( rad * cos(pi/6), -rad * sin(pi/6))
    b = c( 0,                rad)
    c = c(-rad * cos(pi/6), -rad * sin(pi/6))

    graphics::par(new=TRUE)
    plot(c(0,a[1]),c(0,a[2]), type="l", asp=1,col="black", lwd = 2, main=title, xlab="",ylab="", axes=FALSE, frame.plot=FALSE, xlim=c(-3,3), ylim=c(-3,3))
    graphics::par(new=TRUE)
    plot(c(0,b[1]),c(0,b[2]), type="l", asp=1,col="black", lwd = 2, main=title, xlab="",ylab="", axes=FALSE, frame.plot=FALSE, xlim=c(-3,3), ylim=c(-3,3))
    graphics::par(new=TRUE)
    plot(c(0,c[1]),c(0,c[2]), type="l", asp=1,col="black", lwd = 2, main=title, xlab="",ylab="", axes=FALSE, frame.plot=FALSE, xlim=c(-3,3), ylim=c(-3,3))

    graphics::text(a[1] + .2, a[2] - .2, labels = labels[1],  cex=1.0)
    graphics::text(b[1], b[2] + .2, labels = labels[3],  cex=1.0)
    graphics::text(c[1] - .2, c[2] - .2, labels = labels[2],   cex=1.0)
  }

  if (zones==TRUE) {
    nrows = 50
    ncirc = 4

    for (i in 1:ncirc) {
      r = i/ncirc
      circle = matrix(, nrow=nrows+1, ncol=2)
      for (j in 1:nrows) {
        circle[j,1] = rad*r * cos(2 * pi * (j-1) / nrows)
        circle[j,2] = rad*r * sin(2 * pi * (j-1) / nrows)
      }
      circle[nrows+1,] = circle[1,]
      graphics::par(new=TRUE)
      plot(circle[,1:2],type="l", asp=1,col="#9f9f9f",xlab="",ylab="", axes=FALSE, frame.plot=FALSE, xlim=c(-3,3), ylim=c(-3,3))
      graphics::text(rad*r * cos(pi/4) + .1, rad*r * sin(pi/4) + .1, labels = r, cex=0.5)
    }
  }

  if (lobes==TRUE) {
    lobe = matrix(, nrow = 22, ncol = 2)
    for (i in 1:21)
      lobe[i,] = rad*c(cos((i-1)*pi/60),-sin((i-1)*pi/60))
    lobe[22,] = c(0,0)

    graphics::par(new=TRUE)
    graphics::polygon(lobe[,1:2], col=grDevices::rgb(1,0,0,0.2),border=FALSE)

    lobe = matrix(, nrow = 22, ncol = 2)
    for (i in 1:21)
      lobe[i,] = rad*c(cos(pi/3+(i-1)*pi/60),sin(pi/3+(i-1)*pi/60))
    lobe[22,] = c(0,0)

    graphics::par(new=TRUE)
    graphics::polygon(lobe[,1:2], col=grDevices::rgb(0,0,1,0.2),border=FALSE)

    lobe = matrix(, nrow = 22, ncol = 2)
    for (i in 1:21)
      lobe[i,] = rad*c(cos(pi/1.5+(i-1)*pi/60),-sin(pi/1.5+(i-1)*pi/60))
    lobe[22,] = c(0,0)

    graphics::par(new=TRUE)
    graphics::polygon(lobe[,1:2], col=grDevices::rgb(0,1,0,0.2),border=FALSE)

    a1 = c(-rad*cos(pi/3),-rad*sin(pi/3))
    b1 = c( rad*cos(pi/3),-rad*sin(pi/3))
    a2 = c( rad*cos(pi/3), rad*sin(pi/3))
    b2 = c(-rad*cos(pi/3), rad*sin(pi/3))
    graphics::par(new=TRUE)
    plot(c(a1[1],a2[1]),c(a1[2],a2[2]), type="l", lty=3, asp=1,col="#9f9f9f", main=title, xlab="",ylab="", axes=FALSE, frame.plot=FALSE, xlim=c(-3,3), ylim=c(-3,3))
    graphics::par(new=TRUE)
    plot(c(b1[1],b2[1]),c(b1[2],b2[2]), type="l", lty=3, asp=1,col="#9f9f9f", main=title, xlab="",ylab="", axes=FALSE, frame.plot=FALSE, xlim=c(-3,3), ylim=c(-3,3))
  }
}
