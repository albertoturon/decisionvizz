#' @title Frame for 3D iso diagrams
#' @description The function creates the frame for a 3D iso diagram.
#' @param title is the title of the diagram
#' @param pov is the zenith angle for the viewer
#' @param valpha is the longitudinal angle for the viewer
#' @param labels are the labels of the four axes
#' @param axes indicates whether to draw the axes or not
#' @param zones indicates whether to draw the zones or not
#' @param lobes indicates whether to draw the lobes or not
#' @details This function creates the frame for a 3D iso diagram. It is used by the functions that create 3D iso diagrams.
#' @author Alberto Turón \email{turon@@unizar.es}
#' @seealso \code{\link{frame2d}, \link{frame2d_iso}, \link{frame3d}}
frame3d_iso <- function(title=NULL, labels=c("A1","A2","A3","A4"),pov=0,valpha=0.7,zones=TRUE,lobes=TRUE,axes=TRUE) {

  res = 50
  res2 = res*res
  r = 1

  # Esferas
  if (zones == TRUE) {
    X       <- seq(0, pi, length.out = res)
    Y       <- seq(0, 2*pi, length.out = res)
    M       <- plot3D::mesh(X, Y)
    phi     <- M$x
    theta   <- M$y

    x <- sin(phi) * cos(theta)
    y <- cos(phi)
    z <- sin(phi) * sin(theta)

    rgl::surface3d(x, y, z, col="#ffffff", box = FALSE, add=TRUE, alpha=valpha/6)
    rgl::surface3d(2*x/3, 2*y/3, 2*z/3, col="#ffffff", box = FALSE, add=TRUE, alpha=valpha/6)
    rgl::surface3d(x/3, y/3, z/3, col="#ffffff", box = FALSE, add=TRUE, alpha=valpha/6)
  }

  # Lobes
  if (lobes==TRUE) {
    X       <- seq(0, 2*pi, length.out = res)
    Y       <- seq(0, 1, length.out = res)
    M       <- plot3D::mesh(X, Y)
    th  <- M$x
    h   <- M$y

    z0=z <- h * sin(pi/6) * cos(th)
    y0=y <- h * sin(pi/6) * sin(th)
    x0=x <- h

    v=ilr3d(c(1,0,0,0))
    mod=v[1]/sqrt(v[1]^2+v[2]^2+v[3]^2)
    M=rgl::rotationMatrix(-acos(mod),0,v[3],-v[2])

    for (i in 1:res2) {
      v=c(x[i],y[i],z[i],1)
      p=M%*%v
      x0[i]=p[1]
      y0[i]=p[2]
      z0[i]=p[3]
    }

    rgl::surface3d(x0, y0, z0, col="#ff0000", box = FALSE, add=TRUE, alpha=valpha/6)

#    M=rotationMatrix(2*pi/3,-1,1,0)
    v=ilr3d(c(0,1,0,0))
    mod=v[1]/sqrt(v[1]^2+v[2]^2+v[3]^2)
    M=rgl::rotationMatrix(-acos(mod),0,v[3],-v[2])

    for (i in 1:res2) {
      v=c(x[i],y[i],z[i],1)
      p=M%*%v
      x0[i]=p[1]
      y0[i]=p[2]
      z0[i]=p[3]
    }

    rgl::surface3d(x0, y0, z0, col="#00ff00", box = FALSE, add=TRUE, alpha=valpha/6)

    v=ilr3d(c(0,0,1,0))
    mod=v[1]/sqrt(v[1]^2+v[2]^2+v[3]^2)
    M=rgl::rotationMatrix(-acos(mod),0,v[3],-v[2])

    for (i in 1:res2) {
      v=c(x[i],y[i],z[i],1)
      p=M%*%v
      x0[i]=p[1]
      y0[i]=p[2]
      z0[i]=p[3]
    }

    rgl::surface3d(x0, y0, z0, col="#0000ff", box = FALSE, add=TRUE, alpha=valpha/6)

    M=rgl::rotationMatrix(2*pi/3,0,1,0)

    for (i in 1:res2) {
      v=c(x0[i],y0[i],z0[i],1)
      p=M%*%v
      x0[i]=p[1]
      y0[i]=p[2]
      z0[i]=p[3]
    }

    rgl::surface3d(-x, -y, -z, col="#ff00ff", box = FALSE, add=TRUE, alpha=valpha/6)
  }

  # Axes
  if (axes==TRUE) {
    v = ilr3d(c(1,0,0,0))
    linex = c(0,r*v[1])
    liney = c(0,r*v[2])
    linez = c(0,r*v[3])

    linepx = c(0,r*v[1]*1.1)
    linepy = c(0,r*v[2]*1.1)
    linepz = c(0,r*v[3]*1.1)

    rgl::plot3d(linex,liney,linez,type="l", col="#ff0000",add=TRUE, box=FALSE, axes=FALSE)
    rgl::text3d(linepx[2], linepy[2], linepz[2], labels[1], cex=1.2)

#    v1=c(linex[1],liney[1],linez[1],1)
#    v2=c(linex[2],liney[2],linez[2],1)
#    M=rotationMatrix(2*pi/3,0,0,1)
#    p1=M%*%v1
#    p2=M%*%v2
#    linex=c(p1[1],p2[1])
#    liney=c(p1[2],p2[2])
#    linez=c(p1[3],p2[3])

#    linepx=1.1*linex
#    linepy=1.1*liney
#    linepz=1.1*linez

    v = ilr3d(c(0,1,0,0))
    linex = c(0,r*v[1])
    liney = c(0,r*v[2])
    linez = c(0,r*v[3])

    linepx = c(0,r*v[1]*1.1)
    linepy = c(0,r*v[2]*1.1)
    linepz = c(0,r*v[3]*1.1)

    rgl::plot3d(linex,liney,linez,type="l", col="#00ff00",add=TRUE, box=FALSE, axes=FALSE)
    rgl::text3d(linepx[2], linepy[2], linepz[2], labels[2], cex=1.2)

#    v1=c(linex[1],liney[1],linez[1],1)
#    v2=c(linex[2],liney[2],linez[2],1)
#    M=rotationMatrix(2*pi/3,0,1,0)
#    p1=M%*%v1
#    p2=M%*%v2
#    linex=c(p1[1],p2[1])
#    liney=c(p1[2],p2[2])
#    linez=c(p1[3],p2[3])

#    linepx=1.1*linex
#    linepy=1.1*liney
#    linepz=1.1*linez

    v = ilr3d(c(0,0,1,0))
    linex = c(0,r*v[1])
    liney = c(0,r*v[2])
    linez = c(0,r*v[3])

    linepx = c(0,r*v[1]*1.1)
    linepy = c(0,r*v[2]*1.1)
    linepz = c(0,r*v[3]*1.1)

    rgl::plot3d(linex,liney,linez,type="l", col="#0000ff",add=TRUE, box=FALSE, axes=FALSE)
    rgl::text3d(linepx[2], linepy[2], linepz[2], labels[3], cex=1.2)

#    v1=c(linex[1],liney[1],linez[1],1)
#    v2=c(linex[2],liney[2],linez[2],1)
#    M=rotationMatrix(2*pi/3,0,1,0)
#    p1=M%*%v1
#    p2=M%*%v2
#    linex=c(p1[1],p2[1])
#    liney=c(p1[2],p2[2])
#    linez=c(p1[3],p2[3])

#    linepx=1.1*linex
#    linepy=1.1*liney
#    linepz=1.1*linez

    v = ilr3d(c(0,0,0,1))
    linex = c(0,r*v[1])
    liney = c(0,r*v[2])
    linez = c(0,r*v[3])

    linepx = c(0,r*v[1]*1.1)
    linepy = c(0,r*v[2]*1.1)
    linepz = c(0,r*v[3]*1.1)

    rgl::plot3d(linex,liney,linez,type="l", col="#ff00ff",add=TRUE, box=FALSE, axes=FALSE)
    rgl::text3d(linepx[2], linepy[2], linepz[2], labels[4], cex=1.2)
  }
}
