#'@name pathdistGen
#'@title Generate a stack of path distance raster objects
#'@author Joseph Stachelek
#'@param spdf SpatialPointsDataFrame object
#'@param costras RasterLayer cost raster
#'@param range numeric. Range of interpolation neighborhood
#'@param yearmon character. String specifying the name of the spdf
#'@return RasterStack object of path distances
#'@import raster
#'@import gdistance
#'@importFrom utils setTxtProgressBar
#'@export
#'@examples
#'spdf<-data.frame(rnorm(2))
#'xy<-data.frame(x=c(4,2),y=c(8,4))
#'coordinates(spdf)<-xy
#'
#'m<-matrix(NA,10,10)
#'costras<-raster(m,xmn=0,xmx=ncol(m),ymn=0,ymx=nrow(m))
#'costras[]<-runif(ncell(costras),min=1,max=10)
#'#introduce spatial gradient
#'for(i in 1:nrow(costras)){
#'costras[i,]<-costras[i,]+i
#'costras[,i]<-costras[,i]+i
#'}
#'
#'rstack<-pathdistGen(spdf,costras,100)



'pathdistGen'<-function(spdf,costras,range,yearmon="default"){
  
  
  ipdw.range<-range/raster::res(costras)[1]/2 #this is a per cell distance
  #print(ipdw.range)
  
  #start interpolation#####
  #calculate conductances hence 1/max(x)
  trans<-gdistance::transition(costras,function(x)1/max(x),directions=16)
  i=1
  coord<-spdf[i,]
  #print(coord)
  #print(coordinates(coord))
  #print(coord@data)
  A<-gdistance::accCost(trans,coord)
  dist<-hist(A,plot=F)$breaks[2]
  if(dist<ipdw.range){
    dist=ipdw.range    
  }
  
  pb<-utils::txtProgressBar(max=nrow(spdf),style=3)
  
    for(i in 1:nrow(spdf)){
      coord<-spdf[i,]
      #print(coord)
      #print(coordinates(coord))
      #print(coord@data)
      A<-gdistance::accCost(trans,coord)
      A2<-raster::reclassify(A,c(dist,+Inf,NA,ipdw.range,dist,ipdw.range)) #raster cells are 1 map unit
      A3<-((ipdw.range/A2)^2)
      A4<-raster::reclassify(A3,c(-Inf,1,0))
      #print(i)
      #check output with - zoom(A4,breaks=seq(from=0,to=5,by=1),col=rainbow(5))
      #showTmpFiles()
      
      rf<-raster::writeRaster(A4,filename=file.path(tempdir(),paste(yearmon,"A4ras",i,".grd",sep="")),overwrite=T,NAflag=-99999)
      setTxtProgressBar(pb,i)      
    }
  close(pb)
     
  
  #create raster stack
  raster_data<-list.files(path=file.path(tempdir()),pattern=paste(yearmon,"A4ras*",sep=""),full.names=T)
  raster_data<-raster_data[grep(".grd",raster_data,fixed=T)]
  as.numeric(gsub('.*A4ras([0123456789]*)\\.grd$','\\1',raster_data))->fileNum
  raster_data<-raster_data[order(fileNum)]
  rstack<-raster::stack(raster_data)
  rstack<-raster::reclassify(rstack,cbind(-99999,NA))
  file.remove(list.files(path=file.path(tempdir()),pattern=paste(yearmon,"A4ras*",sep=""),full.names=T))
  
  return(rstack)
}