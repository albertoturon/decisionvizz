#' @title Frame for 2D ternary diagrams
#' @description The function creates the frame for a 2D ternary diagram.
#' @param title is the title of the diagram
#' @param labels are the labels of the three axes
#' @param axes indicates whether to draw the axes or not
#' @param zones indicates whether to draw the zones or not
#' @param lobes indicates whether to draw the lobes or not
#' @details This function creates the frame for a 2D ternary diagram. It is used by the functions that create 2D ternary diagrams.
#' @author Alberto Turón \email{turon@@unizar.es}
#' @seealso \code{\link{frame2d_iso}, \link{frame3d}, \link{frame3d_iso}}
frame2d <- function(title=NULL,labels=c("A1","A2","A3"),lobes=TRUE,zones=TRUE,axes=TRUE) {

  graphics::plot(0, type="n", main="", xlab="", ylab="", frame.plot=FALSE, axes=FALSE, xlim=c(-0.75,0.75), ylim=c(-0.4,0.85))

  if (axes == TRUE) {
    O = ilr(c(0, 0, 0))
    a = ilr(c(1, 0, 0))
    b = ilr(c(0, 1, 0))
    c = ilr(c(0, 0, 1))
    x = c(a[1], b[1], c[1], a[1])
    y = c(a[2], b[2], c[2], a[2])
    graphics::par(new=TRUE)
    plot(x,y, type="l", main=title, xlab="", ylab="", axes=FALSE, frame.plot=FALSE, xlim=c(-0.75,0.75), ylim=c(-0.4,0.85))

    x = c(O[1], a[1])
    y = c(O[2], a[2])
    graphics::par(new=TRUE)
    plot(x,y, type="l", xlab="", ylab="", axes=FALSE, frame.plot=FALSE, xlim=c(-0.75,0.75), ylim=c(-0.4,0.85))

    x = c(O[1], b[1])
    y = c(O[2], b[2])
    graphics::par(new=TRUE)
    plot(x,y, type="l", xlab="", ylab="", axes=FALSE, frame.plot=FALSE, xlim=c(-0.75,0.75), ylim=c(-0.4,0.85))

    x = c(O[1], c[1])
    y = c(O[2], c[2])
    graphics::par(new=TRUE)
    plot(x,y, type="l", xlab="", ylab="", axes=FALSE, frame.plot=FALSE, xlim=c(-0.75,0.75), ylim=c(-0.4,0.85))

    graphics::text(a[1] + .04, a[2], labels = labels[1],  cex=0.9)
    graphics::text(b[1] - .04, b[2], labels = labels[2],  cex=0.9)
    graphics::text(c[1], c[2] + .04, labels = labels[3],   cex=0.9)
  }

  if (lobes == TRUE) {
    lobe = matrix(, nrow = 100, ncol = 3)
    for (i in 1:100) {
      lobe[i,] = ilr(clr_inv(ilr_inv(c((i-1)*sin(pi/6)/10,(i-1)*cos(pi/6)/10,-(i-1)*(sin(pi/6)+cos(pi/6))/10))))
    }
    graphics::par(new=TRUE)
    plot(lobe[,1:2],type="l", col=grDevices::adjustcolor("#8080ff", alpha=0.4),xlab="",ylab="", axes=FALSE, xlim=c(-0.75,0.75), ylim=c(-0.4,0.85))
    x1 = lobe[,1]
    y1 = lobe[,2]

    lobe = matrix(, nrow = 100, ncol = 3)
    for (i in 1:100) {
      lobe[i,] = ilr(clr_inv(ilr_inv(c(-(i-1)*sin(pi/6)/10,(i-1)*cos(pi/6)/10,(i-1)*(sin(pi/6)-cos(pi/6))/10))))
    }
    graphics::par(new=TRUE)
    plot(lobe[,1:2],type="l", col=grDevices::adjustcolor("#8080ff", alpha=0.4),xlab="",ylab="", axes=FALSE, xlim=c(-0.75,0.75), ylim=c(-0.4,0.85))
    x2 = lobe[,1]
    y2 = lobe[,2]

    graphics::polygon(c(x2, rev(x1)), c(y2, rev(y1)), col = grDevices::adjustcolor("#8080ff", alpha=0.4), lty = 0)

    lobe = matrix(, nrow = 100, ncol = 3)
    for (i in 1:100) {
      lobe[i,] = ilr(clr_inv(ilr_inv(c(-(i-1)*sin(pi/6)/10,-(i-1)*cos(pi/6)/10,(i-1)*(sin(pi/6)+cos(pi/6))/10))))
    }
    graphics::par(new=TRUE)
    plot(lobe[,1:2],type="l", col=grDevices::adjustcolor("#80ff80", alpha=0.4),xlab="",ylab="", axes=FALSE, xlim=c(-0.75,0.75), ylim=c(-0.4,0.85))
    x1 = lobe[,1]
    y1 = lobe[,2]

    lobe = matrix(, nrow = 100, ncol = 3)
    for (i in 1:100) {
      lobe[i,] = ilr(clr_inv(ilr_inv(c(-(i-1)/10,0,(i-1)/10))))
    }
    graphics::par(new=TRUE)
    plot(lobe[,1:2],type="l", col=grDevices::adjustcolor("#80ff80", alpha=0.4),xlab="",ylab="", axes=FALSE, xlim=c(-0.75,0.75), ylim=c(-0.4,0.85))
    x2 = lobe[,1]
    y2 = lobe[,2]

    graphics::polygon(c(x1, rev(x2)), c(y1, rev(y2)), col = grDevices::adjustcolor("#80ff80", alpha=0.4), lty = 0)

    lobe = matrix(, nrow = 100, ncol = 3)
    for (i in 1:100) {
      lobe[i,] = ilr(clr_inv(ilr_inv(c((i-1)*sin(pi/6)/10,-(i-1)*cos(pi/6)/10,(i-1)*(-sin(pi/6)+cos(pi/6))/10))))
    }
    graphics::par(new=TRUE)
    plot(lobe[,1:2],type="l", col=grDevices::adjustcolor("#ff8080", alpha=0.4),xlab="",ylab="", axes=FALSE, xlim=c(-0.75,0.75), ylim=c(-0.4,0.85))
    x1 = lobe[,1]
    y1 = lobe[,2]

    lobe = matrix(, nrow = 100, ncol = 3)
    for (i in 1:100) {
      lobe[i,] = ilr(clr_inv(ilr_inv(c((i-1)/10,0,-(i-1)/10))))
    }
    graphics::par(new=TRUE)
    plot(lobe[,1:2],type="l", col=grDevices::adjustcolor("#ff8080", alpha=0.4),xlab="",ylab="", axes=FALSE, xlim=c(-0.75,0.75), ylim=c(-0.4,0.85))
    x2 = lobe[,1]
    y2 = lobe[,2]

    graphics::polygon(c(x1, rev(x2)), c(y1, rev(y2)), col = grDevices::adjustcolor("#ff8080", alpha=0.4), lty = 0)
  }

  if (zones == TRUE) {
    nrows = 50

    for (i in 1:7) {
      r = i/3
      circle = matrix(, nrow=nrows+1, ncol=3)
      for (j in 1:nrows) {
        a = r * cos(2 * pi * (j-1) / nrows)
        b = r * sin(2 * pi * (j-1) / nrows)
        circle[j,] = ilr(clr_inv(ilr_inv((c(a, b, -(a+b))))))
      }
      circle[nrows+1,] = circle[1,]
      graphics::par(new=TRUE)
      plot(circle[,1:2],type="l", col="#9f9f9f",xlab="",ylab="", axes=FALSE, xlim=c(-0.75,0.75), ylim=c(-0.4,0.85),pch=3)
    }
  }
}
