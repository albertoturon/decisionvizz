#' @title Frame for 3D ternary diagrams
#' @description The funcion creates the frame for a 3D ternary diagram.
#' @param title is the title of the diagram
#' @param bg is the background color
#' @param valpha is the longitudinal angle for the viewer
#' @param labels are the labels of the four axes
#' @param axes indicates whether to draw the axes or not
#' @param zones indicates whether to draw the zones or not
#' @param lobes indicates whether to draw the lobes or not
#' @param planes indicates whether to draw the internal planes or not
#' @details This funcion creates the frame for a 3D ternary diagram. It is used by the functions that create 3D ternary diagrams.
#' @author Alberto Turón \email{turon@@unizar.es}
#' @seealso \code{\link{frame2d}, \link{frame2d_iso}, \link{frame3d_iso}}
frame3d <- function(title=NULL, labels=c("A1","A2","A3","A4"),bg="white",valpha=0.7,zones=TRUE,lobes=TRUE,axes=TRUE,planes=FALSE) {

#  bg3d(bg) # Setup the background color

  if (axes == TRUE) {
    v1 = ilr3d(c(1,0,0,0))
    v2 = ilr3d(c(0,1,0,0))
    v3 = ilr3d(c(0,0,1,0))
    v4 = ilr3d(c(0,0,0,1))
    O  = ilr3d(c(0,0,0,0))

    vp1 =ilr3d(c(1.1,0,0,0))
    vp2 =ilr3d(c(0,1.1,0,0))
    vp3 =ilr3d(c(0,0,1.1,0))
    vp4 =ilr3d(c(0,0,0,1.1))

    vertices = c(
      v1[1], v1[2], v1[3], 1.0,
      v2[1], v2[2], v2[3], 1.0,
      v3[1], v3[2], v3[3], 1.0,
      v4[1], v4[2], v4[3], 1.0
    )

    indices = c( 1, 2, 3,
                 1, 2, 4,
                 1, 3, 4,
                 2, 3, 4
    )

    rgl::wire3d( rgl::qmesh3d(vertices, indices) )
    rgl::text3d(vp1[1], vp1[2], vp1[3], labels[1], cex=1.2)
    rgl::text3d(vp2[1], vp2[2], vp2[3], labels[2], cex=1.2)
    rgl::text3d(vp3[1], vp3[2], vp3[3], labels[3], cex=1.2)
    rgl::text3d(vp4[1], vp4[2], vp4[3], labels[4], cex=1.2)

    rgl::plot3d(c(O[1],v1[1]),c(O[2],v1[2]),c(O[3],v1[3]),type="l",col="#777777",add=TRUE)
    rgl::plot3d(c(O[1],v2[1]),c(O[2],v2[2]),c(O[3],v2[3]),type="l",col="#777777",add=TRUE)
    rgl::plot3d(c(O[1],v3[1]),c(O[2],v3[2]),c(O[3],v3[3]),type="l",col="#777777",add=TRUE)
    rgl::plot3d(c(O[1],v4[1]),c(O[2],v4[2]),c(O[3],v4[3]),type="l",col="#777777",add=TRUE)
  }

  # Lobes
  if (lobes == TRUE) {
    X   <- seq(0, 2*pi, length.out = 50)
    Y   <- seq(0, 10, length.out = 50)
    M   <- plot3D::mesh(X, Y)
    th  <- M$x
    h   <- M$y

    x <- h
    y <- h * sin(pi/6) * sin(th)
    z <- h * sin(pi/6) * cos(th)

    for (i in 1:50) {
      for (j in 1:50) {
        d = ilr3d(clr_inv((c(x[i,j],y[i,j],z[i,j],-(x[i,j]+y[i,j]+z[i,j])))))
        x[i,j] = d[1]
        y[i,j] = d[2]
        z[i,j] = d[3]
      }
    }

    rgl::surface3d(x, y, z, col="#ff0000", box = FALSE, add=TRUE, alpha=valpha/6)

    M=rgl::rotationMatrix(-2*pi/3,v3[1],v3[2],v3[3])

    for (i in 1:2500) {
      v=c(x[i],y[i],z[i],1)
      p=M%*%v
      x[i]=p[1]
      y[i]=p[2]
      z[i]=p[3]
    }

    rgl::surface3d(x, y, z, col="#00ff00", box = FALSE, add=TRUE, alpha=valpha/6)

    M=rgl::rotationMatrix(-2*pi/3,v1[1],v1[2],v1[3])

    for (i in 1:2500) {
      v=c(x[i],y[i],z[i],1)
      p=M%*%v
      x[i]=p[1]
      y[i]=p[2]
      z[i]=p[3]
    }

    rgl::surface3d(x, y, z, col="#0000ff", box = FALSE, add=TRUE, alpha=valpha/6)

    M=rgl::rotationMatrix(-2*pi/3,v1[1],v1[2],v1[3])

    for (i in 1:2500) {
      v=c(x[i],y[i],z[i],1)
      p=M%*%v
      x[i]=p[1]
      y[i]=p[2]
      z[i]=p[3]
    }

    rgl::surface3d(x, y, z, col="#ff00ff", box = FALSE, add=TRUE, alpha=valpha/6)
  }

  # Esferas
  if (zones == TRUE) {
    thres = 30
    phres = 15

    X = seq(0, 2*pi, length.out = phres)
    Y = seq(0, pi, length.out = thres)
    M = plot3D::mesh(X,Y)
    u = M$x
    v = M$y

    for (k in 1:7) {
      r = k/3

      x = r * sin(v) * cos(u)
      y = r * sin(v) * sin(u)
      z = r * cos(v)

      for (i in 1:phres) {
        for (j in 1:thres) {
          d = ilr3d(clr_inv(c(x[i,j],y[i,j],z[i,j],-(x[i,j]+y[i,j]+z[i,j]))))
          x[i,j] = d[1]
          y[i,j] = d[2]
          z[i,j] = d[3]
        }
      }
      rgl::surface3d(x, y, z, col="#ffffff", box = FALSE, add=TRUE, alpha=valpha/6)
    }
  }

  # Planes

  if (planes) {
    # Pruebo
    v1m=(v2+v4)/2
    x = c(v1[0],v1m[0],v4[0])
    y = c(v1[1],v1m[1],v4[1])
    z = c(v1[2],v1m[2],v4[2])
    rgl::triangles3d(x,y,z,col="yellow")
  }
}
