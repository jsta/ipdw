#'@name costrasterGen
#'@title Generate a cost Raster
#'@description Generate a cost raster from an object of class \code{SpatialPolygons}, \code{matrix}, or \code{SpatialPointsDataFrame}
#'@author Joseph Stachelek
#'@param xymat Matrix of coordinates or a \code{SpatialPointsDataFrame} object
#'@param pols SpatialPolygons object
#'@param extent Define extent based on extent of xymat/xyspdf (points) or pols (polys). Default is polys.
#'@param projstr proj4 string defining the inherent projection
#'@details Ensure that the projection of the xymat coordinates and pols match. This can be accomplished by running the \code{projection} command on both inputs. If they do not match use the \code{spTransform} command.
#'@seealso \code{\link[rgdal]{spTransform-methods}}, \code{\link[raster]{rasterize}}
#'@return RasterLayer
#'@import raster
#'@import sp
#'@export
#'@examples
#'Sr1<-Polygon(cbind(c(0,0,2,2,0),c(0,4,4,0,0)))
#'Sr2<-Polygon(cbind(c(2,2,4,4,2),c(0,2,2,0,0)))
#'Srs1<-Polygons(list(Sr1), "s1")
#'Srs2<-Polygons(list(Sr2), "s2")
#'pols<-SpatialPolygons(list(Srs1,Srs2), 1:2)
#'
#'
#'#using a matrix object
#'xymat<-matrix(3,3,nrow=1,ncol=2)
#'costras<-costrasterGen(xymat,pols,projstr=NULL)
#'
#'
#'#plotting
#'plot(costras)
#'points(xymat)

'costrasterGen'<-function(xymat,pols,extent="polys",projstr){
  
if(class(xymat)=="SpatialPointsDataFrame"|class(xymat)=="SpatialPoints"){
  xymat<-coordinates(xymat)
}

#add check to see if projstr and projection(pols) match
if(!identical(projstr,projection(pols))&&class(xymat)!="matrix"){
  message("Warning, the projection of polygons does not match projstr. See rgdal::spTransform")
}

#define spatial domain based on pnts or polys
if(extent=="polys"){
  xmn<-min(sp::bbox(pols)[1,])
  xmx<-max(sp::bbox(pols)[1,])
  ymn<-min(sp::bbox(pols)[2,])
  ymx<-max(sp::bbox(pols)[2,])
}

if(extent=="points"|extent=="pnts"){
  ymn<-range(xymat[,2])[1]
  ymx<-range(xymat[,2])[2]
  xmn<-range(xymat[,1])[1]
  xmx<-range(xymat[,1])[2]
}
  
nrow<-ymx-ymn
ncol<-xmx-xmn
  
#generate cost raster####
  r<-raster::raster(nrow=nrow,ncol=ncol,crs=projstr,xmn=xmn,xmx=xmx,ymn=ymn,ymx=ymx)
  costras<-rasterize(pols,r,silent=TRUE)
  m <- c(0, +Inf, 10000)
  rclmat <- matrix(m, ncol=3, byrow=TRUE)
  costras2<-raster::reclassify(costras,rclmat)
  costras3<-raster::reclassify(costras2,cbind(NA,1))
  
  return(costras3)  
}

