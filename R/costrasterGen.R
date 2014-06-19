#'@name costrasterGen
#'@title Generate a cost Raster
#'@description generate a cost raster from an object of class "SpatialPolygons"
#'@author Joseph Stachelek
#'@param xymat Matrix of coordinates
#'@param pols SpatialPolygons object
#'@param extent Define extent based on extent of xymat (pnts) or pols (polys). Default is polys.
#'@param projstr proj4 string defining the inherent projection
#'@return RasterLayer
#'@import raster
#'@export
#'@examples
#'Sr1<-Polygon(cbind(c(0,0,2,2,0),c(0,4,4,0,0)))
#'Sr2<-Polygon(cbind(c(2,2,4,4,2),c(0,2,2,0,0)))
#'Srs1<-Polygons(list(Sr1), "s1")
#'Srs2<-Polygons(list(Sr2), "s2")
#'pols<-SpatialPolygons(list(Srs1,Srs2), 1:2)
#'
#'xymat<-matrix(3,3,nrow=1,ncol=2)
#'costras<-costrasterGen(xymat,pols,projstr=NULL)
#'plot(costras)
#'points(xymat)

'costrasterGen'<-function(xymat,pols,extent="polys",projstr){
  
#define spatial domain based on pnts or polys
if(extent=="polys"){
  xmn<-min(bbox(pols)[1,])
  xmx<-max(bbox(pols)[1,])
  ymn<-min(bbox(pols)[2,])
  ymx<-max(bbox(pols)[2,])
}

if(extent=="pnts"){
  ymn<-range(xymat[,2])[1]
  ymx<-range(xymat[,2])[2]
  xmn<-range(xymat[,1])[1]
  xmx<-range(xymat[,1])[2]
}
  
nrow<-ymx-ymn
ncol<-xmx-xmn
  
#generate cost raster####
  r<-raster(nrow=nrow,ncol=ncol,crs=projstr,xmn=xmn,xmx=xmx,ymn=ymn,ymx=ymx)
  costras<-rasterize(pols,r,silent=TRUE)
  m <- c(0, +Inf, 10000)
  rclmat <- matrix(m, ncol=3, byrow=TRUE)
  costras2<-reclassify(costras,rclmat)
  costras3<-reclassify(costras2,cbind(NA,1))
  
  return(costras3)  
}

